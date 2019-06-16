;;;; utilities.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq)


(defun sanitize-name (string-designator)
  "Convert a stringifiable STRING-DESIGNATOR into a snake-case string."
  (substitute #\_ #\- (string-downcase (string string-designator))))

(defun str->lisp-keyword (str)
  "Convert a snake-case string into a hyphenated Lisp keyword."
  (make-keyword (substitute #\- #\_ (string-upcase str))))

(defun format-log (s fmt-string &rest args)
  "Writes a format string to the stream S in debug output format."
  (format s "[~A | ~A] ~?~%" (local-time:now) (bt:thread-name (bt:current-thread)) fmt-string args))

(defun unpack-foreign-msg-to-bytes (msg)
  "Converts a foreign array of unsigned characters to a Lisp vector of such."
  (cffi:foreign-array-to-lisp (pzmq:msg-data msg)
                              `(:array :uint8 ,(pzmq:msg-size msg))))

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
