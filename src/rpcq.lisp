;;;; rpcq.lisp
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

(in-package #:rpcq)

;;; contains tools for manipulating and (de-)serializing messages
;;; that are passed around the Rigetti core stack.


;; store all messages defined thus far
(defvar *messages* nil)

(defun clear-messages ()
  "Clear the stored message definitions."
  (setf *messages* nil))


(deftype atom-type ()
  '(member :string :bytes :bool :float :integer :any))


(defparameter *python-types*
  '(:string "str"
    :bytes "bytes"
    :float "float"
    :integer "int"
    :bool "bool"
    :map "dict"
    :list "list"
    :any "object"))

(defparameter *python-instance-check-types*
  '(:string "basestring"
    :bytes "bytes"
    :float "float"
    :integer "int"
    :bool "bool"
    :map "dict"
    :list "list"
    :any "object"))

(defun format-documentation-string (string)
  "Format a documentation string STRING into its final stored representation.

The input strings are assumed to be FORMAT-compatible, so sequences like ~<newline> are allowed."
  (check-type string string)
  (format nil string))

(defun python-instance-check-type (field-type)
  (let ((*python-types* *python-instance-check-types*))
    (python-type field-type)))

(defun to-octets (string)
  "Convert a string S to a vector of 8-bit unsigned bytes"
  (map 'simple-vector 'char-code string))

(defun to-string (octets)
  "Convert a vector of octets to a string"
  (map 'string 'code-char octets))

(defun python-argspec-default (field-type default)
  "Translate DEFAULT values for immutable objects of a given
FIELD-TYPE to python."
  (case field-type
    ((:string)
     (if default
         (format nil "~S" default)
         "None"))
    ((:bytes)
     (if default
         (format nil "b~S" (to-string default))
         "None"))
    ((:bool)
     (if default
         "True"
         "False"))
    ((:integer)
     (if default
         (format nil "~d" default)
         "None"))
    ((:float)
     (if default
         (format nil "~e" default)
         "None"))
    (otherwise
     "None")))

(defun python-type (field-type)
  "Always return a basic python type not List[...] or Dict[...] for
instance checks."
  (python-typing-type
   (if (listp field-type)
       (car field-type)
       field-type)))

(defun python-typing-type (field-type)
  "Return the python typing-module compliant field type"
  (cond
    ((keywordp field-type)
     (assert (member field-type *python-types*)
             (field-type)
             "Unknown field-type ~S" field-type)
     (getf *python-types* field-type))
    ((symbolp field-type)
     ;; field-type is assumed to be message object
     (format nil "~a" field-type))
    ((eq (car field-type) :list)
     (format nil "List[~a]" (python-typing-type (cadr field-type))))
    ((eq (car field-type) :map)
     (assert (string= (symbol-name (caddr field-type)) "->")
             (field-type)
             "Bad mapping spec.")
     (format nil
             "Dict[~a,~b]"
             (python-typing-type (cadr field-type))
             (python-typing-type (cadddr field-type))))))

(defun python-maybe-optional-typing-type (field-type required)
  "Rerturn the python type string for FIELD-TYPE while
accounting for whether the field is REQUIRED.
"
  (let ((b (python-typing-type field-type)))
    (if (or required
            (listp field-type))
        b
        (format nil "Optional[~a]" b))))

(defun python-collections-initform (field-type default)
  "Translate a DEFAULT value of type FIELD-TYPE to a python initform."
  (check-type field-type list)
  (cond
    ;; handle lists
    ((eq :list (car field-type))
     (if (null default)
         "[]"
         (with-output-to-string (s)
           (yason:encode default s))))

    ;; handle mappings
    ((eq :map (car field-type))
     (if (null default)
         "{}"
         (with-output-to-string (s)
           (yason:encode (%plist-to-string-hash-table default) s))))
    (t
     (error "Unrecognized field-type ~A" field-type))))


(defun python-message-spec (&optional (stream nil))
  "print an importable python file with the message definitions"
  (format stream "#!/usr/bin/env python

\"\"\"
WARNING: This file is auto-generated, do not edit by hand. See README.md.
\"\"\"

import warnings
from rpcq._base import Message
from typing import List, Dict, Optional

# Python 2/3 str/unicode compatibility
from past.builtins import basestring


class CoreMessages(object):
    \"\"\"
    WARNING: This class is auto-generated, do not edit by hand. See README.md.
    This class is also DEPRECATED.
    \"\"\"


class _deprecated_property(object):

    def __init__(self, prop):
        self.prop = prop

    def __get__(self, *args):
        warnings.warn(
            \"'CoreMessages.{0}' is deprecated. Please access '{0}' directly at the module level.\".format(
                self.prop.__name__),
            UserWarning)
        return self.prop

")
  ;; for each message we need:
  ;; - the message name and documentation
  ;; - the message field names to define the
  ;;   slots as well as the asdict and astuple methods
  ;; - the constructor definition argument specification
  ;;   (includes default values for primitive types)
  ;; - the constructor type signature
  ;; - the default values for lists and dicts
  ;; - the not-None checks for all required arguments
  ;; - the type checks for all arguments (accounting for required args)
  ;; - the instance attribute assignment plus attribute docstring
  (loop :for (msg-name field-specs documentation) :in *messages*
        :collect
        (let
            (slot-names
             args-no-default
             args-with-default
             required-args
             field-instantiations
             type-checks
             collections-defaults)
          (loop :for (name . params) :in (reverse field-specs)
                :for required := (getf params :required)
                :for type := (getf params :type)
                :for typing-type := (python-maybe-optional-typing-type type required)
                :for basic-type := (python-type type)
                :for instance-check-type := (python-instance-check-type type)
                :for defaultp := (member :default params)
                :for default := (getf params :default)
                :for collectionp := (listp type)
                :for documentation := (getf params :documentation
                                            (format nil "Field ~a of type ~a" name type))
                :do
                   ;; slot names
                   (push name slot-names)

                   ;; constructor arguments
                   (if (and required
                            (not defaultp))

                       ;; regular positional argument
                       (push (list name typing-type) args-no-default)

                       ;; keyword argument
                       (push (list
                              (format nil "~a=~a" name
                                      (python-argspec-default
                                       type
                                       default))
                              typing-type)
                             args-with-default))

                   ;; required arguments that are not collections with
                   ;; default values
                   (when (and required
                              (or
                               (not defaultp)
                               (not collectionp)))
                     (push name required-args))

                   ;; instance attribute initializations
                   (push (list name typing-type documentation) field-instantiations)

                   ;; type checking
                   (push
                    (list required name instance-check-type name instance-check-type name)
                    type-checks)

                   ;; Initialize default values for collections
                   (when (and collectionp
                              (or (not required)
                                  defaultp))
                     (push (list
                            name
                            (python-collections-initform type default))
                           collections-defaults)))
          (let
              ;; concatenate positional and keyword args
              ((init-arg-spec
                 (concatenate 'list
                              (mapcar #'car args-no-default)
                              (mapcar #'car args-with-default)))

               ;; constructor argument type signature
               (typing-types-arg-spec
                 (concatenate 'list
                              (mapcar #'cadr args-no-default)
                              (mapcar #'cadr args-with-default))))

            ;; Add class header, documentation and implementations of
            ;; asdict and astuple
            (format stream
                    "~&
class ~A(Message):
    \"\"\"~a\"\"\"

    # fix slots
    __slots__ = (
        ~{'~a'~^,
        ~},
    )

    def asdict(self):
        \"\"\"Generate dictionary representation of self.\"\"\"
        return {
            ~:*~{'~a~:*': self.~a~^,
            ~}
        }

    def astuple(self):
        \"\"\"Generate tuple representation of self.\"\"\"
        return (
            ~:*~{self.~a~^,
            ~}
        )~%"
                    (symbol-name msg-name)
                    (or documentation (symbol-name msg-name))
                    slot-names)

            ;; Add constructor signature and type hint
            (format stream  "
    def __init__(self,
                 ~{~a~^,
                 ~},
                 **kwargs):
        # type: (~{~a~^, ~}) -> None~%
        if kwargs:
            warnings.warn((\"Message {} ignoring unexpected keyword arguments: \"
                    \"{}.\").format(self.__class__.__name__, \", \".join(kwargs.keys())))
"
                    init-arg-spec
                    typing-types-arg-spec)

            ;; Initialize collections (list+dict) that either have
            ;; default values or are optional to empty containers if
            ;; they are None
            (when collections-defaults
              (format stream "
        ~:[~;# initialize default values of collections~]~:*
        ~{if ~a is None:
            ~:*~a = ~a~^
        ~}~%"
                      (apply #'append collections-defaults)))

            ;; Check that required fields are not None (skip
            ;; collections with default values as those are
            ;; automatically converted to empty containers above
            (when required-args
              (format stream "
        ~:[~;# check presence of required fields~]~:*
        ~{if ~a~:* is None:
            raise ValueError(\"The field '~a' cannot be None\")~^
        ~}~%"
                      required-args))

            ;; Verify field types. For non-required fields a value of
            ;; None is permitted
            (when type-checks
              (format stream "
        ~:[~;# verify types~]~:*
        ~{if not ~:[(~a is None or isinstance(~:*~a, ~a))~;isinstance(~a, ~a)~]:
            raise TypeError(\"Parameter ~a must be of type ~a, \"
                            + \"but object of type {} given\".format(type(~a)))~^
        ~}~%"
                      (apply #'append type-checks)))

            ;; Actually set the instance attributes and add type hint
            ;; and docstring
            (format stream "
        ~{self.~a~:* = ~a  # type: ~a
        \"\"\"~a\"\"\"~^

        ~}~%"
                    (apply #'append field-instantiations))
            (format stream "
CoreMessages.~A = _deprecated_property(~:*~A)
" (symbol-name msg-name))))))

(defun serialize (obj &optional stream)
  "Serialize OBJ, either written to a stream or returned as a vector of (INTEGER 0 255)."
  (let ((messagepack:*encode-alist-as-map* nil))
    (typecase stream
      (stream
       (messagepack:encode-stream (%serialize obj) stream))
      (otherwise
       (messagepack:encode (%serialize obj))))))

(defgeneric %serialize (payload)
  (:documentation "Writes RPCQ objects into JSON-ifiable form, for further passage to msgpack."))

(defmethod %serialize (payload)
  payload)

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
    (array
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


(defmacro defmessage (class-name field-specs &key (documentation nil))
  "Create a (de-)serializable message definition with name CLASS-NAME and slots (SLOT-SPECS)."
  (check-type class-name symbol)
  (setf *messages* (nconc *messages* `((,class-name ,field-specs ,documentation))))
  (flet ((accessor (slot-name)
           (alexandria:symbolicate (symbol-name class-name)
                                   "-"
                                   (symbol-name slot-name))))
    (flet ((make-slot-spec (field-spec)
             (let*
                 ((slot-name (car field-spec))
                  (field-settings (cdr field-spec))
                  (field-type (getf field-settings :type))
                  (required (getf field-settings :required))
                  (documentation (getf field-settings :documentation))
                  (defaultp (member :default field-settings))
                  (default (getf field-settings :default)))

               (multiple-value-bind (slot-type initform)
                   (slot-type-and-initform field-type required default)
                 `(,slot-name :initarg ,(intern (symbol-name slot-name) :keyword)
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
           (init-spec (json)
             (lambda (slot-name)
               `( ,(intern (symbol-name slot-name) :keyword)
                  (%deserialize (gethash ,(symbol-name slot-name) ,json))))))
      (alexandria:with-gensyms (obj hash-table)
        (let ((slot-names (mapcar #'car field-specs)))
          `(progn
             (defclass ,class-name ()
               ,(mapcar #'make-slot-spec field-specs)
               ,@(when documentation
                   `((:documentation ,(format-documentation-string documentation)))))
             
             (defmethod %serialize ((,obj ,class-name))
               (with-slots ,slot-names ,obj
                 (let ((,hash-table (make-hash-table :test #'equal)))
                   (setf (gethash "_type" ,hash-table) ,(symbol-name class-name))
                   ,@(loop :for slot :in slot-names
                           :collect `(setf (gethash ,(symbol-name slot) ,hash-table)
                                           (%serialize ,slot)))
                   ,hash-table)))

             (defmethod %deserialize-struct ((type (eql ',class-name)) (payload hash-table))
               (assert (string= (gethash "_type" payload) ,(symbol-name class-name)))
               (make-instance ',class-name
                              ,@(mapcan (init-spec 'payload) slot-names)))


             (defmethod print-object ((,obj ,class-name) stream)
               (print-unreadable-object (,obj stream :type t)
                 (pprint-indent :block 2)
                 ,@(loop :for slot :in slot-names
                         :collect `(format stream
                                           "~&  ~A -> ~S"
                                           ,(symbol-name slot)
                                           (,(accessor slot) ,obj)))))))))))
