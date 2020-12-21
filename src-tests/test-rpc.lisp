;;;; test-rpc.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:rpcq-tests)

;; TODO FIXME XXX The below relies on some funky/hacky/unsightly code
;; to forcefully kill threads. See open issues
;; https://github.com/rigetti/rpcq/issues/61
;; https://github.com/rigetti/rpcq/issues/75

(defun kill-thread-slowly (thread)
  #+ccl
  (loop :while (bt:thread-alive-p thread)
        :do (sleep 1) (bt:destroy-thread thread))
  #-ccl
  (when (bt:thread-alive-p thread)
    (bt:destroy-thread thread))
  (sleep 1))

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
        (kill-thread-slowly server-thread)))))

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
        (kill-thread-slowly server-thread)))))

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
        (kill-thread-slowly server-thread)))))

(deftest test-RPCRequest-client_timeout-overrides-server-timeout ()
  ;; If the client provides a timeout, it should override the server
  ;; timeout, and the server should then kill jobs that exceed this
  ;; timeout. To test this, make a request with a client timeout of 2,
  ;; the request job sleeps for 4 seconds and then raises an error. If
  ;; the server correctly kills the job, the error should not be
  ;; raised.
  (with-unique-rpc-address (addr)
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler
                  dt
                  (lambda ()
                    (sleep 4)
                    ;; Shouldn't hit this if the client timeout was
                    ;; respected.
                    (error "oof"))
                  :name "test-method")
                 (rpcq:start-server :dispatch-table dt
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           (rpcq:with-rpc-client (client addr :timeout 2)
             (signals bt:timeout
               (rpc-call client "test-method")))
        ;; kill the server thread
        (kill-thread-slowly server-thread)))))

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
        (kill-thread-slowly server-thread)))))

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
        (kill-thread-slowly server-thread)))))

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
          (kill-thread-slowly server-thread))))))

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
        (kill-thread-slowly server-thread)))))

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
                                              (rpcq::serialize (make-instance 'deserialize-bomb 'messagepack-sym:id 9)))))
             (is (search "Threw generic error before RPC call"
                         (get-output-stream-string log-stream))))
        ;; kill the server thread
        (kill-thread-slowly server-thread)))))

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
        (kill-thread-slowly server-thread)))))


;; Canonical test keys from the the ZMQ curve docs
;; http://api.zeromq.org/4-3:zmq-curve#toc4
(defvar *server-public-key-z85* "rq:rM>}U?@Lns47E1%kR.o@n%FcmmsL/@{H8]yf7")
(defvar *server-secret-key-z85* "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6")
(defvar *client-public-key-z85* "Yne@$w-vo<fVvi]a<NY6T1ed:M$fCG*[IaLV{hID")
(defvar *client-secret-key-z85* "D:)Q[IlAW!ahhC2ac:9*A}h:p?([4%wOTJ%JR%cs")

(defun random-in-range (low high)
  "Return a random number in the range [low, high)."
  (assert (< low high))
  (+ low (random (- high low))))

(deftest test-client-server-dialogue-with-curve-auth ()
  ;; Curve-enabled sockets require TCP transport. We could bind on wildcard port, but that would
  ;; require some way for START-SERVER to inform us what ephemeral port was handed out. Instead,
  ;; we bind to a random port in the 10,000-12,000 range and pray.
  (let ((addr (format nil "tcp://127.0.0.1:~D" (random-in-range 10000 12000))))
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :auth-config (rpcq:make-server-auth-config
                                                  :server-secret-key *server-secret-key-z85*)
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           (rpcq:with-rpc-client (client addr :auth-config (rpcq:make-client-auth-config
                                                            :server-public-key *server-public-key-z85*
                                                            :client-public-key *client-public-key-z85*
                                                            :client-secret-key *client-secret-key-z85*))
             (let ((server-response (rpcq:rpc-call client "test-method")))
               (is (string= *expected-response* server-response))))
        ;; kill the server thread
        (kill-thread-slowly server-thread)))))

(deftest test-client-server-dialogue-with-invalid-curve-auth ()
  ;; Curve-enabled sockets require TCP transport. Switching to inproc:// causes ZeroMQ to silently
  ;; ignore any configured security mechanism (!), meaning the connection will succeed, even with
  ;; invalid auth keys. As far as I can tell, this behavior is not explicitly documented anywhere in
  ;; the zmq docs, and it took longer than I want to admit to figure this out!
  (let ((addr (format nil "tcp://127.0.0.1:~D" (random-in-range 10000 12000)))
        (invalid-server-public-key "qq:rM>}U?@Lns47E1%kR.o@n%FcmmsL/@{H8]yf7"))
    (let* ((server-function
             (lambda ()
               (let ((dt (rpcq:make-dispatch-table)))
                 (rpcq:dispatch-table-add-handler dt 'test-method)
                 (rpcq:start-server :dispatch-table dt
                                    :auth-config (rpcq:make-server-auth-config
                                                  :server-secret-key *server-secret-key-z85*)
                                    :listen-addresses (list addr)))))
           (server-thread (bt:make-thread server-function)))
      (sleep 1)
      (unwind-protect
           (rpcq:with-rpc-client (client addr :timeout 1
                                              :auth-config (rpcq:make-client-auth-config
                                                            :server-public-key invalid-server-public-key
                                                            :client-public-key *client-public-key-z85*
                                                            :client-secret-key *client-secret-key-z85*))
             ;; An invalid server key will result in a hung connection, where the server keeps
             ;; closing the connection and the client keeps trying to reconnect with the same
             ;; keys. The ZMTP spec does provide an ERROR command that would allow the server to
             ;; indicate a fatal error, but it seems that this mechanism is not used by ZeroMQ in
             ;; this case. From the ZMTP spec[1]:
             ;;
             ;;     ZMTP allows an explicit fatal error response during the mechanism handshake,
             ;;     using the ERROR command. The peer SHALL treat an incoming ERROR command as
             ;;     fatal, and act by closing the connection, and not re-connecting using the same
             ;;     security credentials.
             ;;
             ;; Instead, when the server fails to decrypt the signature box in the client HELLO
             ;; message, it just closes the connection, which the client (correctly) takes as an
             ;; invitation to try again:
             ;;
             ;;     An implementation SHOULD signal any other error, e.g. overloaded, temporarily
             ;;     refusing connections, etc. by closing the connection. The peer SHALL treat an
             ;;     unexpected connection close as a temporary error, and SHOULD reconnect.
             ;;
             ;; Closing the connection rather than sending an ERROR response appears to an explicit
             ;; choice on ZeroMQ's part[2].
             ;;
             ;; [1]: https://rfc.zeromq.org/spec/23/#error-handling
             ;; [2]: https://github.com/zeromq/libzmq/issues/2227#issuecomment-269760343
             (signals bt:timeout (rpcq:rpc-call client "test-method")))
        ;; kill the server thread
        (kill-thread-slowly server-thread)))))
