;;;; utilities.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq)


(defun sanitize-name (string-designator)
  "Convert a stringifiable STRING-DESIGNATOR into a snake-case string."
  (substitute #\_ #\- (string string-designator)))

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
