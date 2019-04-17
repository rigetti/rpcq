#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload :rpcq)

; messages
(rpcq::clear-messages :messages)
(load "src/messages.lisp")
(with-open-file (f "rpcq/messages.py"
                   :direction ':output
                   :if-exists ':supersede)
  (rpcq::python-message-spec f (gethash :messages rpcq::*messages*))
  (write-line "Wrote new messages.py"))

; core-messages
(rpcq::clear-messages :core-messages)
(load "src/core-messages.lisp")
(with-open-file (f "rpcq/core-messages.py"
                   :direction ':output
                   :if-exists ':supersede)
  (rpcq::python-message-spec f (gethash :core-messages rpcq::*messages*))
  (write-line "Wrote new core-messages.py"))
