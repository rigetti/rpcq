#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)
(rpcq::clear-messages)

(dolist (namespace-and-parents '(;; table of pairs: (module-name (parent-module-python-name1 ...))
                                 ("messages" nil)
                                 ("core-messages" ("rpcq.messages"))))
  (destructuring-bind (namespace parent-namespaces) namespace-and-parents
    (load (make-pathname :name namespace :type "lisp" :directory '(:relative "src")))
    (let ((path (make-pathname :name (rpcq::sanitize-name namespace) :type "py" :directory '(:relative "rpcq"))))
      (with-open-file (f path
                         :direction ':output
                         :if-exists ':supersede
                         :if-does-not-exist ':create)
        (rpcq::python-message-spec f (gethash namespace rpcq::*messages*) parent-namespaces)
        (format t "Wrote new ~A.py~%" (pathname-name path))))))
