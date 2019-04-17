#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)
(rpcq::clear-messages)

(dolist (namespace (list "messages" "core-messages"))
  (load (format nil "src/~A.lisp" namespace))
  (with-open-file (f (format nil "rpcq/~A.py" namespace)
                     :direction ':output
                     :if-exists ':supersede)
    (rpcq::python-message-spec f (gethash namespace rpcq::*messages*))
    (write-line (format nil "Wrote new ~A.py" namespace))))
