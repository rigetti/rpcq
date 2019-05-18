#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)
(ql:quickload :split-sequence)
(rpcq::clear-messages)

(dolist (namespace '("messages" "core-messages"))
  (load (make-pathname :name namespace :type "lisp" :directory '(:relative "src")))
  (let ((path (make-pathname :name (rpcq::sanitize-name namespace) :type "py" :directory '(:relative "rpcq"))))
    (with-open-file (f path
                     :direction ':output
                     :if-exists ':supersede
                     :if-does-not-exist ':create)
      (rpcq::python-message-spec f (gethash namespace rpcq::*messages*))
      (format t "Wrote new ~A.py~%" (pathname-name path)))))
