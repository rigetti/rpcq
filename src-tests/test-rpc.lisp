;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)


(defparameter *expected-response* "test-response")

(defun test-method (&key (sleep 1 slee-p))
  (when slee-p
    (sleep sleep))
  "test-response")

(defmacro with-unique-rpc-address ((addr) &body body)
  `(let ((,addr (concatenate 'string "inproc://" (format nil "~a" (uuid:make-v4-uuid)))))
     ,@body))

(deftest test-client-server-dialogue ()
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; hook up the client
           (rpcq:with-rpc-client (client addr)
             ;; send a communique
             (let ((server-response (rpcq:rpc-call client "test-method")))
               (is (string= *expected-response* server-response))))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))

(deftest test-client-timeout ()
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; hook up the client
           (rpcq:with-rpc-client (client addr :timeout 1)
             ;; send a communique
             (signals bt:timeout
               (rpcq:rpc-call client "test-method" :sleep 5)))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))

(deftest test-server-timeout ()
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :timeout 1
                                    :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; hook up the client
           (rpcq:with-rpc-client (client addr)
             ;; send a communique
             (signals rpcq::rpc-error
               (rpcq:rpc-call client "test-method" :sleep 5)))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))

(defun served-method ()
  (warn "The purpose of this test is to communicate a warning.")
  "Some other reply payload.")

(deftest test-server-warnings ()
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'served-method)
                 (rpcq:start-server :timeout 5
                                    :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; hook up the client
           (rpcq:with-rpc-client (client addr)
             ;; send a communique
             (signals simple-warning
               (is (string= "Some other reply payload."
                            (rpcq:rpc-call client "served-method")))))
        ;; kill the server thread
        (bt:destroy-thread server-thread)))))
