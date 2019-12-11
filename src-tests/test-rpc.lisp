;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)

(defun make-test-logger (&optional (stream *error-output*))
  (make-instance 'cl-syslog:rfc5424-logger
                 :app-name "rpcq-tests"
                 :facility ':local0
                 :maximum-priority ':err
                 :log-writer (cl-syslog:stream-log-writer stream)))

;; stolen from quilc
(defmacro special-bindings-let* (let-defs &body body)
  "Bind LET-DEFS as in LET, and add those LET-DEFS to bordeaux-threads:*default-special-bindings* in the same LET."
  `(let* (,@(loop :for (name value) :in let-defs
                  :collect `(,name ,value))
          (bordeaux-threads:*default-special-bindings*
            (list* ,@(loop :for (name value) :in let-defs
                           :collect `(cons ',name (list 'quote ,name)))
                   bordeaux-threads:*default-special-bindings*)))
     ,@body))


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

(defun error-with-serious-condition ()
  (error 'serious-condition))

(defun error-with-error-condition ()
  (error "owie!"))

(deftest test-error-handling ()
  "Test that various types of processing errors are handled correctly."
  (dolist (rpc-method '("error-with-serious-condition"
                        "error-with-error-condition"
                        "unknown-method"))
    (with-unique-rpc-address (addr)
      (let* ((server-function
               (lambda ()
                 (let ((dt (rpcq:make-dispatch-table)))
                   (rpcq:dispatch-table-add-handler dt 'error-with-serious-condition)
                   (rpcq:start-server :dispatch-table dt
                                      :listen-addresses (list addr)))))
             (server-thread (bt:make-thread server-function)))
        (sleep 1)
        (unwind-protect
             ;; hook up the client
             (rpcq:with-rpc-client (client addr)
               ;; send a communique
               (signals rpc-error
                 (rpcq:rpc-call client rpc-method)))
          ;; kill the server thread
          #+ccl
          (loop :while (bt:thread-alive-p server-thread)
                :do (sleep 1) (bt:destroy-thread server-thread))
          #-ccl
          (bt:destroy-thread server-thread))))))

(deftest test-invalid-rpc-request ()
  "Test that invalid RPC requests are handled correctly."
  (with-unique-rpc-address (addr)
    (let* ((log-stream (make-string-output-stream))
           (server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)
                                    :logger (make-test-logger log-stream)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; The invalid request will be ignored, resulting in no response being sent, so we
           ;; instead test that the request times out.
           (rpcq:with-rpc-client (client addr :timeout 1)
             (signals bt:timeout
               (rpcq::%rpc-call-raw-request client
                                            (princ-to-string (uuid:make-v4-uuid))
                                            ;; not a valid |RPCRequest|
                                            (make-array 8 :element-type '(unsigned-byte 8)
                                                          :initial-element 0))
               (is (search "Threw generic error before RPC call"
                           (get-output-stream-string log-stream)))))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))

(deftest test-server-deserialize-error ()
  "Test that deserialization errors are handled correctly."
  (with-unique-rpc-address (addr)
    (let* ((log-stream (make-string-output-stream))
           (server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)
                                    :logger (make-test-logger log-stream)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           ;; The invalid request will be ignored, resulting in no response being sent, so we
           ;; instead test that the request times out.
           (rpcq:with-rpc-client (client addr :timeout 1)
             (signals bt:timeout
               (rpcq::%rpc-call-raw-request client
                                            (princ-to-string (uuid:make-v4-uuid))
                                            ;; Bind MESSAGEPACK:*EXTENDED-TYPES* here, which allows
                                            ;; us to serialize the extended type. Since the bindings
                                            ;; aren't in effect for the rpc server, this will result
                                            ;; in an error when messagepack attempts to deserialize
                                            ;; the unknown extended type.
                                            (let ((messagepack:*extended-types*
                                                    (messagepack:define-extension-types
                                                        '(0 deserialize-bomb))))
                                              (rpcq::serialize (make-instance 'deserialize-bomb :id 9)))))
             (is (search "Threw generic error before RPC call"
                         (get-output-stream-string log-stream))))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))

(defun oof-find-me-on-the-stack ()
  (error "oof!"))

(defun test-error-method ()
  (oof-find-me-on-the-stack))

(deftest test-log-backtrace ()
  (with-unique-rpc-address (addr)
    (let* ((error-stream (make-string-output-stream))
           (log-stream (make-string-output-stream))
           (server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-error-method)
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)
                                    :debug t
                                    :logger (make-test-logger log-stream)))))
           (server-thread (special-bindings-let* ((*error-output* error-stream))
                            ;; We want the *ERROR-OUTPUT* binding to remain in effect for the worker
                            ;; threads created by SERVER-THREAD (grandchildren of the current
                            ;; thread), hence we add BT:*DEFAULT-SPECIAL-BINDINGS* to itself.
                            (special-bindings-let* ((bt:*default-special-bindings* bt:*default-special-bindings*))
                              (bt:make-thread server-function)))))
      (sleep 1)
      (unwind-protect
           (rpcq:with-rpc-client (client addr)
             (signals rpcq::rpc-error
               (rpcq:rpc-call client "test-error-method"))
             (is (search "Unhandled error in host program" (get-output-stream-string log-stream)))
             (is (search "OOF-FIND-ME-ON-THE-STACK" (get-output-stream-string error-stream))))
        ;; kill the server thread
        #+ccl
        (loop :while (bt:thread-alive-p server-thread)
              :do (sleep 1) (bt:destroy-thread server-thread))
        #-ccl
        (bt:destroy-thread server-thread)))))
