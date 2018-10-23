#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)
(rpcq::clear-messages)
(load "src/messages.lisp")
(with-open-file (f "rpcq/messages.py"
                   :direction ':output
                   :if-exists ':supersede) 
  (rpcq::python-message-spec f)
  (write-line "Wrote new message spec."))

