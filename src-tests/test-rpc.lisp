;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)


(defparameter *expected-response* "test-response")

(defun test-method (&key (sleep 1 slee-p))
  (when slee-p
    (sleep sleep))
  "test-response")

;; TODO FIXME XXX The below relies on some funky/hacky/unsightly code
;; to forcefully kill threads. See open issues
;; https://github.com/rigetti/rpcq/issues/61
;; https://github.com/rigetti/rpcq/issues/75

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
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))

(deftest test-param-order ()
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'list)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; hook up the client
           (rpcq:with-rpc-client (client addr)
             ;; send a communique
             (let ((server-response (rpcq:rpc-call client "list" 1 2 3 :four "fore!")))
               (is (equalp server-response #(1 2 3 "four" "fore!")))))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
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
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
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
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
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
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))

(deftest test-unknown-method ()
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
           (rpcq:with-rpc-client (client addr)
             (signals rpcq::rpc-error
               (rpcq:rpc-call client "no-such-method")))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))

(defun test-error-method ()
  (error "oof!"))

(deftest test-log-backtrace ()
  (with-unique-rpc-address (addr)
    (let* ((log-stream (make-string-output-stream))
           (server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-error-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)
                                    :debug t
                                    :logger (make-instance 'cl-syslog:rfc5424-logger
                                                           :app-name "rpcq-tests"
                                                           :facility ':local0
                                                           :maximum-priority ':err
                                                           :log-writer
                                                           (cl-syslog:stream-log-writer log-stream))))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           (rpcq:with-rpc-client (client addr)
             (signals rpcq::rpc-error
               (rpcq:rpc-call client "test-error-method"))
             (is (search "TRIVIAL-BACKTRACE:PRINT-BACKTRACE" (get-output-stream-string log-stream))))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))
