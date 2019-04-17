#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)
(rpcq::clear-messages)

(dolist (namespace '("messages" "core-messages"))
  (load (make-pathname :name namespace :type "lisp" :directory '(:relative "src")))
  (with-open-file (f (make-pathname :name namespace :type "py" :directory '(:relative "rpcq"))
                     :direction ':output
                     :if-exists ':supersede)
    (rpcq::python-message-spec f (gethash namespace rpcq::*messages*))
    (format t "Wrote new ~A.py~%" namespace)))
