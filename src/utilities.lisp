;;;; utilities.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq)


(defun sanitize-name (string-designator)
  "Convert a stringifiable STRING-DESIGNATOR into a snake-case string."
  (substitute #\_ #\- (string-downcase (string string-designator))))

(defun snake-to-kebab (str)
  "Convert STR from snake_case to KEBAB-CASE. Yum!"
  (string-upcase (substitute #\- #\_ str)))

(defun camel-to-kebab (str)
  "Convert STR from CamelCase to KEBAB-CASE. Yum!"
  (let ((raw-segments (cl-ppcre:all-matches-as-strings "(^[a-z]|[A-Z0-9])[a-z]*"
                                                       str))
        (running-singletons nil)
        (kebab-segments nil))
    (loop :for seg :in raw-segments
          :if (= 1 (length seg))
            :do (push seg running-singletons)
          :else
            :do (progn
                  (when running-singletons
                    (push (apply #'concatenate 'string
                                 (nreverse running-singletons))
                          kebab-segments)
                    (setf running-singletons nil))
                  (push seg kebab-segments))
          :finally (when running-singletons
                     (push (apply #'concatenate 'string
                                  (nreverse running-singletons))
                           kebab-segments)))

    (format nil "~{~:@(~A~)~^-~}" (nreverse kebab-segments))))

(defun str->lisp-keyword (str)
  "Convert a snake-case string into a hyphenated Lisp keyword."
  (make-keyword (snake-to-kebab str)))

(defun format-log (s fmt-string &rest args)
  "Writes a format string to the stream S in debug output format."
  (format s "[~A | ~A] ~?~%" (local-time:now) (bt:thread-name (bt:current-thread)) fmt-string args))

(defun unpack-foreign-msg-to-bytes (msg)
  "Converts a foreign array of unsigned characters to a Lisp vector of such."
  (flet ((copy-foreign-bytes (pointer len)
           (let ((lisp-vector (cffi:make-shareable-byte-vector len)))
             (cffi:with-pointer-to-vector-data (vec-ptr lisp-vector)
               (cffi:foreign-funcall "memcpy" :pointer vec-ptr :pointer pointer :size len :pointer)
               lisp-vector))))
    (copy-foreign-bytes (pzmq:msg-data msg) (pzmq:msg-size msg))))

(defun global-function-p (symbol)
  "Return true if SYMBOL is a symbol naming a global function. Return false otherwise."
  (and (typep symbol 'symbol)
       (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defmacro with-unique-rpc-address ((addr) &body body)
  "Bind ADDR in the context of BODY to a unique address acceptable to RPCQ:START-SERVER."
  `(let ((,addr (format nil "inproc://~a" (uuid:make-v4-uuid))))
     ,@body))
