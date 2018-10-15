;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)


(deftest test-client-server-dialogue ()
  (let* ((expected-response "test-response")
         (method-name "test-method")
         (server-thread
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt (constantly expected-response)
                                                :name method-name)
               (rpcq:start-server :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test"))))))
    ;; spawn the server thread
    (let ((server-thread (bt:make-thread server-thread)))
      (unwind-protect
           (progn
             (sleep 1)
             ;; hook up the client
             (rpcq:with-rpc-client (client "inproc://RPCQ-test")
               ;; send a communique
               (let ((server-response (rpcq:rpc-call client method-name)))
                 (is (string= expected-response server-response)))))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))

(deftest test-client-timeout ()
  (let* ((method-name "test-method")
         (server-thread
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt (lambda () (sleep 5))
                                                :name method-name)
               (rpcq:start-server :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test"))))))
    ;; spawn the server thread
    (let ((server-thread (bt:make-thread server-thread)))
      (unwind-protect
           (progn
             (sleep 1)
             ;; hook up the client
             (rpcq:with-rpc-client (client "inproc://RPCQ-test" :timeout 1)
               ;; send a communique
               (signals sb-ext:timeout
                 (rpcq:rpc-call client method-name))))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))

(deftest test-server-timeout ()
  (let* ((method-name "test-method")
         (server-thread
           (lambda ()
             (let ((dt (rpcq:make-dispatch-table)))
               (rpcq:dispatch-table-add-handler dt (lambda () (sleep 5))
                                                :name method-name)
               (rpcq:start-server :timeout 1
                                  :dispatch-table dt
                                  :listen-addresses '("inproc://RPCQ-test"))))))
    ;; spawn the server thread
    (let ((server-thread (bt:make-thread server-thread)))
      (unwind-protect
           (progn
             (sleep 1)
             ;; hook up the client
             (rpcq:with-rpc-client (client "inproc://RPCQ-test")
               ;; send a communique
               (signals rpcq::rpc-error
                 (rpcq:rpc-call client method-name))))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))
