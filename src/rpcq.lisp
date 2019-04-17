;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2018 Rigetti Computing
;;;;
;;;;    Licensed under the Apache License, Version 2.0 (the "License");
;;;;    you may not use this file except in compliance with the License.
;;;;    You may obtain a copy of the License at
;;;;
;;;;        http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;;    Unless required by applicable law or agreed to in writing, software
;;;;    distributed under the License is distributed on an "AS IS" BASIS,
;;;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;;    See the License for the specific language governing permissions and
;;;;    limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; rpcq.lisp
;;;;

(in-package #:rpcq)

;;; contains tools for manipulating and (de-)serializing messages
;;; that are passed around the Rigetti core stack.


(defvar *namespace* nil)

(defmacro in-namespace (namespace)
  (setf *namespace* namespace))

;; store all messages defined thus far in their namespace
(defvar *messages* (make-hash-table))

(defun clear-messages (namespace)
  "Clear the stored message definitions."
  (setf (gethash namespace *messages*) nil))


(deftype atom-type ()
  '(member :string :bytes :bool :float :integer :any))


(defun format-documentation-string (string)
  "Format a documentation string STRING into its final stored representation.

The input strings are assumed to be FORMAT-compatible, so sequences like ~<newline> are allowed."
  (check-type string string)
  (format nil string))

(defun to-octets (string)
  "Convert a string S to a vector of 8-bit unsigned bytes"
  (flexi-streams:string-to-octets string :external-format ':utf8))

(defun to-string (octets)
  "Convert a vector of octets to a string"
  (flexi-streams:octets-to-string octets :external-format ':utf8))


(defun serialize (obj &optional stream)
  "Serialize OBJ, either written to a stream or returned as a vector of (INTEGER 0 255)."
  (let ((messagepack:*encode-alist-as-map* nil))
    (etypecase stream
      (stream
       (messagepack:encode-stream (%serialize obj) stream))
      (null
       (messagepack:encode (%serialize obj))))))

(defgeneric %serialize (payload)
  (:documentation "Writes RPCQ objects into JSON-ifiable form, for further passage to msgpack."))

(defmethod %serialize (payload)
  payload)

(defmethod %serialize ((payload cons))
  (cond
    ((alexandria:proper-list-p payload)
     (loop :for elt :in payload :collect (%serialize elt)))
    (t
     (error "Can only serialize proper lists, not raw conses. Got ~S" payload))))

(defmethod %serialize ((payload hash-table))
  (let ((hash (make-hash-table :test #'equal)))
    (loop :for k :being :the :hash-keys :of payload
            :using (hash-value v)
          :do (setf (gethash (%serialize k) hash) (%serialize v)))
    hash))

(defgeneric %deserialize (payload)
  (:documentation "Reconstruct objects that have already been converted to Lisp objects."))

(defgeneric %deserialize-struct (type payload))

(defmethod %deserialize ((payload cons))
  (loop :for elt :in payload :collect (%deserialize elt)))

(defmethod %deserialize ((payload string))
  payload)

(defmethod %deserialize ((payload array))
  (%deserialize (coerce payload 'list)))

(defmethod %deserialize ((payload hash-table))
  (let ((type (gethash "_type" payload)))
    (if type
        (%deserialize-struct (intern type :rpcq) payload)
        (let ((result (make-hash-table :test 'equal)))
          (loop :for k :being :the :hash-keys :of payload
                :using (hash-value v)
                :do (setf (gethash k result) (%deserialize v)))
          result))))

(defmethod %deserialize (payload)
  payload)

(defun deserialize (payload)
  "Deserialize the object(s) encoded in PAYLOAD (string or stream)."
  (etypecase payload
    (vector
     (%deserialize (messagepack:decode payload)))
    (stream
     (%deserialize (messagepack:decode-stream payload)))))

(defun slot-type-and-initform (field-type required default)
  "Translate a FIELD-TYPE to a Lisp type and initform taking into account
whether the field is REQUIRED and a specified DEFAULT value.

The primitive field types must be specified as one of

  :string :bytes :integer :float :bool

Message field types are specified by their class name, e.g. for
a Calibration message the field type is

  |Calibration|

List and mapping field types are specified as

  (:list x) (:map :string -> x)

where x is one of
{:string, :bytes, :any, :integer, :float, :bool, :list, :mapping} or a
message field type. We currently only support :string keys for
mappings as JSON does not support other kinds of keys but we
nonetheless explicitly include the key type for better readability

If a field is non-required, it may be None on the python side or NIL
in Lisp.

We distinguish between the following options for any field type:
1. Required with no default
    - invalid not to provide a value for this field
    - invalid to pass None/null/NIL for this field
2. Required with a default
    - if a value is not provided, then a fallback value will be used
    - invalid to pass None/null/NIL for this field
3. Optional with no default (equivalent to optional with a default of None in python,
   null in JSON, NIL in Lisp)
    - valid to either not provide a value or to pass None for this field
4. Optional with a default
    - if a value is not provided, then a fallback value is used
    - you can explicitly pass None/null/NIL for this field
"
  (cond

    ;; handle :string :integer :float :bool :bytes
    ((keywordp field-type)
     (check-type field-type atom-type)
     (let*
         ((basic-type (getf '(:string (simple-array character)
                              :bytes (simple-array (unsigned-byte 8))
                              :integer fixnum
                              :float double-float
                              :bool boolean
                              :any t)
                            field-type))
          ;; make sure the default value (if defined) is coerced
          ;; to correct type
          (coerced-default (when default
                             (if (and (eq :bytes field-type) (typep default 'string))
                                 ;; accept a string as the default value for a bytes object
                                 (to-octets default)
                                 (coerce default basic-type)))))

       (if required
           (values basic-type coerced-default)

           ;; else also allow NIL
           (values `(or null ,basic-type) coerced-default))))

    ;; handle defined message types
    ((symbolp field-type)
     (if required
         (values `(or null ,field-type) default)
         (values field-type default)))

    ;; handle lists
    ((eq ':list (car field-type))
     ;; Need not check if REQUIRED as NIL is still of type list
     ;; We use 'simple-vector rather than 'list as this maps better to
     ;; the JSON distinction betweel null and []
     (values 'simple-vector (coerce default 'simple-vector)))

    ;; handle mappings
    ((eq ':map (car field-type))
     (let ((initform
             (if default

                 ;; default should be specified as plist
                 `(%plist-to-string-hash-table ',default)
                 '(make-hash-table :test 'equalp))))
       (if required
           (values 'hash-table initform)
           (values '(or null hash-table) initform))))
    (t
     (error "Unrecognized field-type ~A" field-type))))


(defun map-plist (f plist)
  "Iterate over a PLIST and call F with key and value as parameters."
  (loop :for (k v) :on plist :by #'cddr
        :do (funcall f k v)))

(defun %plist-to-string-hash-table (plist)
  "Generate a hash-table from a PLIST while simultaneously converting the keys to strings."
  (let ((tbl (make-hash-table :test 'equalp)))
    (map-plist (lambda (k v)
                 (setf (gethash (symbol-name k) tbl) v))
               plist)
    tbl))


(defmacro defmessage (class-name parent-name field-specs &key (documentation nil))
  "Create a (de-)serializable message definition.

PARAMETERS:
  * CLASS-NAME: The name of the message type.
  * PARENT-NAME: A list of length at most 1 of \"parent message types\". Child message types inherit their parents slot specifications.
  * FIELD-SPECS: A list of slot specifications for the message type.  An entry in FIELD-SPECS is a plist with the following possible keys:
    :TYPE          - Either an atomic type, a list type, a map type, or another RPCQ message name.
    :REQUIRED      - A boolean indicating whether the field is required. If it is required, there must be a default value, either supplied through :DEFAULT or implicitly.
    :DOCUMENTATION - A documentation string.
    :DEFAULT       - A default value for the slot.
    :DEPRECATES    - The slot name that this slot replaces.
    :DEPRECATED-BY - The slot name that this slot is replaced by.
    :DEPRECATED    - Indicates that this slot is being phased out without a replacement.

LIMITATIONS:
  * DEPRECATES / DEPRECATED-BY must come in balanced pairs.
  * DEPRECATES / DEPRECATED-BY are mutually exclusive. (Nested deprecation is not currently supported.)
  * DEPRECATES / DEPRECATED-BY must reference slots on the same message, and not on parent messages."
  (check-type class-name symbol)
  (assert (or (null parent-name)
              (and (typep parent-name 'cons)
                   (= 1 (length parent-name)))))
  (assert (not (null *namespace*)) (list *namespace*) "No active namespace set, use IN-NAMESPACE")
  (let ((messages (gethash *namespace* *messages*)))
    (setf (gethash *namespace* *messages*) (nconc messages `((,class-name ,(first parent-name) ,field-specs ,documentation)))))
  (labels ((accessor (slot-name)
             (alexandria:symbolicate (symbol-name class-name)
                                     "-"
                                     (symbol-name slot-name)))
           (make-slot-spec (field-spec)
             (let*
                 ((slot-name (car field-spec))
                  (field-settings (cdr field-spec))
                  (field-type (getf field-settings ':type))
                  (required (getf field-settings ':required))
                  (documentation (getf field-settings ':documentation))
                  (defaultp (member ':default field-settings))
                  (default (getf field-settings ':default))
                  (deprecated (getf field-settings ':deprecated))
                  (deprecates (getf field-settings ':deprecates))
                  (deprecated-by (getf field-settings ':deprecated-by)))

               (assert (not (and deprecates deprecated-by))
                       ()
                       "It is currently unsupported for messages to have multiple levels of deprecation.")

               (assert (not (and deprecated (or deprecates deprecated-by)))
                       ()
                       "It does not make sense to deprecate a field simultaneously with and without replacement.")

               (multiple-value-bind (slot-type initform)
                   (slot-type-and-initform field-type required default)
                 `(,slot-name
                   ;; a slot can have multiple initialization vectors.
                   ;; we allow a slot to be initialized by its own name, by the
                   ;; name of a slot that it deprecates, and by the name of a slot
                   ;; that it's deprecated by. these override each other: we
                   ;; prefer the slot that it's deprecated by to our own name,
                   ;; which we in turn prefer to the slot which we deprecate.
                   ,@(when deprecates
                       `(:initarg ,(intern (symbol-name deprecates) :keyword)))
                   :initarg ,(intern (symbol-name slot-name) :keyword)
                   ,@(when deprecated-by
                       `(:initarg ,(intern (symbol-name deprecated-by) :keyword)))

                   :reader ,(accessor slot-name)
                   :type ,slot-type

                   ;; only add documentation if present
                   ,@(when documentation
                       (list :documentation (format-documentation-string
                                             documentation)))

                   ;; if no default value given
                   ;; but field is required raise error
                   ,@(if (and required (not defaultp))
                         `(:initform (error
                                      (concatenate
                                       'string
                                       "Missing value for field "
                                       ,(symbol-name slot-name))))
                         ;; else initialize to
                         ;; initform generated by TRANSLATE-FIELD-TYPE
                         `(:initform ,initform))))))
           (slot-initialization-deprecation-warnings (field-spec)
             (let* ((slot-name (car field-spec))
                    (slot-keyword (intern (symbol-name slot-name) :keyword))
                    (field-settings (cdr field-spec)))
               (alexandria:when-let* ((deprecated-by (getf field-settings ':deprecated-by))
                                      (deprecated-by-keyword (intern (symbol-name deprecated-by) :keyword)))
                 (list `(when (and (getf initargs ,slot-keyword)
                                   (not (getf initargs ,deprecated-by-keyword)))
                          (warn ,(format nil "~a has been deprecated by ~a."
                                         slot-keyword deprecated-by-keyword)))))))
           (slot-accessor-deprecation-warnings (field-spec)
             (let* ((slot-name (car field-spec))
                    (slot-keyword (intern (symbol-name slot-name) :keyword))
                    (field-settings (cdr field-spec)))
               (alexandria:when-let* ((deprecated-by (getf field-settings ':deprecated-by))
                                      (deprecated-by-keyword (intern (symbol-name deprecated-by) :keyword)))
                 (list `(defmethod ,(accessor slot-name) ((obj ,class-name))
                          (warn ,(format nil "~a has been deprecated by ~a."
                                         slot-keyword deprecated-by-keyword))
                          (,(accessor deprecated-by) obj))))))
           (init-spec (json)
             (lambda (slot-name)
               `( ,(intern (symbol-name slot-name) :keyword)
                  (%deserialize (gethash ,(symbol-name slot-name) ,json))))))
    (alexandria:with-gensyms (obj init-args)
      (let ((slot-names (mapcar #'car field-specs)))
        `(progn
           ;; here we define the actual message class object.
           ;; the most complicated aspect is setting up all the slot definitions
           ;; (which store default init values, type info, ...).
           (defclass ,class-name ,parent-name
             ,(mapcar #'make-slot-spec field-specs)
             ,@(when documentation
                 `((:documentation ,(format-documentation-string documentation)))))

           ;; in the event that our message has deprecated slots, we need to
           ;; warn the user if they use them during initialization.
           (defmethod shared-initialize ((instance ,class-name) slot-names
                                         &rest initargs
                                         &key &allow-other-keys)
             (declare (ignore initargs))
             ,@(mapcan #'slot-initialization-deprecation-warnings field-specs)
             (call-next-method))

           ;; similarly, we warn a user who tries to read from deprecated slots
           ,@(mapcan #'slot-accessor-deprecation-warnings field-specs)

           ;; in order to serialize our message, we recurse through our class
           ;; ancestry and load up a hash table with all our slots. this splits
           ;; into two parts: initializing the hash table (done here) and the
           ;; recursion (done in %%serialize)
           (defmethod %serialize ((,obj ,class-name))
             (%%serialize ,obj (make-hash-table :test #'equal)))

           (defmethod %%serialize ((,obj ,class-name) (hash-table hash-table))
             (with-slots ,slot-names ,obj
               ,@(loop :for slot :in slot-names
                       :collect `(setf (gethash ,(symbol-name slot) hash-table)
                                       (%serialize ,slot)))
               (call-next-method)
               (setf (gethash "_type" hash-table) ,(symbol-name class-name))
               hash-table))

           ;; similarly, in order to deserialize a message we recurse over our
           ;; class ancestry and load up a hash table with all of the expected
           ;; slots. this also cleaves into two parts: setting up the hash
           ;; table (done here) and the recursion (done in %%deserialize-struct)
           (defmethod %deserialize-struct ((type (eql ',class-name)) (payload hash-table))
             (assert (string= (gethash "_type" payload) ',class-name))
             (let ((,init-args (%%deserialize-struct type payload)))
               (apply 'make-instance
                      ',class-name
                      ,init-args)))

           (defmethod %%deserialize-struct ((type (eql ',class-name)) (payload hash-table))
             ,(cond
                (parent-name
                 `(list*
                   ,@(mapcan (init-spec 'payload) slot-names)
                   (%%deserialize-struct ',@parent-name payload)))
                (t
                 `(list ,@(mapcan (init-spec 'payload) slot-names)))))

           ;; similarly similarly, in order to print out the contents of an RPCQ
           ;; message we walk over our class ancestry and print out all the
           ;; slots that each ancestor contributes.  this also also cleaves into
           ;; two parts: setting up the basics of the printing (done here) and
           ;; the recursion (done in %print-slots)
           (defmethod print-object ((,obj ,class-name) stream)
             (print-unreadable-object (,obj stream :type t)
               (pprint-indent :block 2)
               (%print-slots ,obj stream)))

           (defmethod %print-slots ((,obj ,class-name) stream)
             ,@(loop :for spec :in field-specs
                     :unless (getf (cdr spec) ':deprecated-by)
                       :collect `(format stream
                                         "~&  ~A -> ~S"
                                         ,(symbol-name (car spec))
                                         (,(accessor (car spec)) ,obj)))
             (call-next-method))

           nil)))))

(defmethod %print-slots (obj stream)
  nil)

(defmethod %%serialize (obj hash-table)
  nil)

(defmethod %%deserialize-struct (type payload)
  nil)
