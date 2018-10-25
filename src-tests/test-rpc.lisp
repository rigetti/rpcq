;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)


(defparameter *expected-response* "test-response")

(defun test-method (&key (sleep 1 sleep-p))
  (when sleep-p
    (sleep sleep))
  "test-response")


(deftest test-client-server-dialogue ()
  (let* ((server-function
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt 'test-method)
               (rpcq:start-server :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test")))))
         (server-thread (bt:make-thread server-function)))
    (sleep 1)
    (unwind-protect
      ;; hook up the client
      (rpcq:with-rpc-client (client "inproc://RPCQ-test")
        ;; send a communique
        (let ((server-response (rpcq:rpc-call client "test-method")))
          (is (string= *expected-response* server-response))))
      ;; kill the server thread
      (bt:destroy-thread server-thread))))

(deftest test-client-timeout ()
  (let* ((server-function
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt 'test-method)
               (rpcq:start-server :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test")))))
         (server-thread (bt:make-thread server-function)))
    (sleep 1)
    (unwind-protect
         ;; hook up the client
         (rpcq:with-rpc-client (client "inproc://RPCQ-test" :timeout 1)
               ;; send a communique
               (signals sb-ext:timeout
                 (rpcq:rpc-call client "test-method" :sleep 5)))
      ;; kill the server thread
      (bt:destroy-thread server-thread))))

(deftest test-server-timeout ()
  (let* ((server-function
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt 'test-method)
               (rpcq:start-server :timeout 1
                                  :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test")))))
         (server-thread (bt:make-thread server-function)))
    (sleep 1)
    (unwind-protect
         ;; hook up the client
         (rpcq:with-rpc-client (client "inproc://RPCQ-test")
               ;; send a communique
               (signals rpcq::rpc-error
                 (rpcq:rpc-call client "test-method" :sleep 5)))
      ;; kill the server thread
      (bt:destroy-thread server-thread))))
