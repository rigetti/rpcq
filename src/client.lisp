;;;; client.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Lisp mimic of the python JSON RPC Client.

;;
;; Suppose someone else has set up a compute resource which supplies access to
;; some valuable functions over RPCQ, which you would like to access.  You're in
;; luck!  The RPCQ client makes this task easy.
;;
;; Suppose that the definition of the function on the remote machine is as such:
;;
;; (defun my-favorite-function (&keys (argA valA) (argB valB) ...)
;;   ...)
;;
;; If you had access to its definition locally, you might invoke it as
;;
;; (my-favorite-function :argA val1 :argB val2 ...)
;;
;; Making the analogous call over an RPC client looks like this:
;;
;; (with-rpc-client (socket "tcp://the-endpoint")
;;   (rpc-call socket "my-favorite-function" :argA val1 :argB val2 ...))
;;
;; The connection to the remote server is automatically opened and closed by
;; the form WITH-RPC-CLIENT, and it can be used as many times as one likes
;; within the body of that form.
;;


(in-package #:rpcq)

(defstruct rpc-client
  "Holds the data for an (active) RPCQ client connection."
  socket
  (timeout nil :type (or null (real 0))))

(defmethod print-object ((object rpc-client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((rpc-client-socket object)
       (format stream "~s" (pzmq:getsockopt (rpc-client-socket object) :last-endpoint)))
      (t
       (format stream "DISCONNECTED")))))


(define-condition rpc-error (simple-error)
  ((error-string :initarg :string :reader rpc-error-string)
   (request-id :initarg :id :reader rpc-error-request-id))
  (:documentation "An RPC call signaled an error on the remote host.")
  (:report (lambda (condition stream)
             (format stream "RPC request ~a resulted in error:~%~a~&"
                     (rpc-error-request-id condition)
                     (rpc-error-string condition)))))

(define-condition rpc-protocol-error (simple-error)
  ((object :initarg :object :reader rpc-protocol-error-object)
   (id :initarg :id :reader rpc-protocol-error-id))
  (:documentation "While listening for an RPC call reply, the client received malformed information.")
  (:report (lambda (condition stream)
             (format stream "RPC request ~a resulted in bad reply:~%~a~&"
                     (rpc-protocol-error-id condition)
                     (rpc-protocol-error-object condition)))))


(defun prepare-rpc-call-args (args)
  (let ((*args (list))
        (**kwargs (make-hash-table :test #'equal)))
    (labels
        ((process-args (args)
           (cond
             ((null args)
              t)
             ((keywordp (first args))
              (process-**kwargs args))
             (t
              (push (first args) *args)
              (process-args (rest args)))))
         
         (process-**kwargs (args)
           (cond
             ((null args)
              t)
             (t
              (let ((key (sanitize-name (first args)))
                    (val (second args)))
                (setf (gethash key **kwargs) val)
                (process-**kwargs (rest (rest args))))))))
      
      (process-args args)
      (alexandria:plist-hash-table
       (list "*args" *args
             "**kwargs" **kwargs)
       :test #'equal))))

(defun rpc-call (client call &rest args)
  "Makes a synchronous RPC call, designated by the string method name CALL, over the connection CLIENT.  ARGS is a plist of arguments.  Returns the result of the call directly."
  (let* ((uuid (unicly:uuid-princ-to-string (unicly:make-v4-uuid)))
         (request (make-instance '|RPCRequest|
                                 :|id| uuid
                                 :|params| (prepare-rpc-call-args args)
                                 :|method| (sanitize-name call)))
         (payload (serialize request))
         (socket (rpc-client-socket client)))
    (cffi:with-foreign-object (foreign-payload :uint8 (length payload))
      (dotimes (j (length payload))
        (setf (cffi:mem-aref foreign-payload ':uint8 j)
              (aref payload j)))
      (pzmq:send socket foreign-payload :len (length payload)))
    ;; NOTE: Here lies a tail-recursive loop that waits for a reply.
    ;;       Lisp users working in an implementation that doesn't handle
    ;;       tail-recursion well should beware that receiving a bunch of bad
    ;;       packets could blow the stack!
    (labels
        ((loop-til-reply ()
           (let (unpacked-reply)
             (pzmq:with-message msg
               (pzmq:msg-recv msg socket)
               (setf unpacked-reply (deserialize (unpack-foreign-msg-to-bytes msg))))
             (typecase unpacked-reply
               (|RPCError|
                (cond
                  ((string= uuid (|RPCError-id| unpacked-reply))
                   (loop :for rpc-warning :across (|RPCError-warnings| unpacked-reply)
                         :do (warn "Warning during RPC call: ~a: ~a"
                                   (|RPCWarning-kind| rpc-warning)
                                   (|RPCWarning-body| rpc-warning)))
                   (error 'rpc-error
                          :string (|RPCError-error| unpacked-reply)
                          :id (|RPCError-id| unpacked-reply)))
                  (t
                   (warn "Discarding RPC error with ID ~a, which doesn't match ours of ~a."
                         (|RPCError-id| unpacked-reply) uuid)
                   (loop-til-reply))))
               (|RPCReply|
                (cond
                  ((string= uuid (|RPCReply-id| unpacked-reply))
                   (loop :for rpc-warning :across (|RPCReply-warnings| unpacked-reply)
                         :do (warn "Warning during RPC call: ~a: ~a"
                                   (|RPCWarning-kind| rpc-warning)
                                   (|RPCWarning-body| rpc-warning)))
                   (|RPCReply-result| unpacked-reply))
                  (t
                   (warn "Discarding RPC error with ID ~a, which doesn't match ours of ~a."
                         (|RPCReply-id| unpacked-reply) uuid)
                   (loop-til-reply))))
               (otherwise
                (error 'rpc-protocol-error
                       :id uuid
                       :object unpacked-reply))))))
      (cond
        ((rpc-client-timeout client)
         (let ((timeout (rpc-client-timeout client)))
           (bt:with-timeout (timeout)
             (loop-til-reply))))
        (t
         (loop-til-reply))))))

(defmacro with-rpc-client ((client endpoint &rest options) &body body)
  "Opens an RPCQ client connection, referenced by CLIENT, at ENDPOINT.  The connection is automatically closed as this form is exited or unwound.  Hence, CLIENT is only valid during the execution of BODY, and it should not be stored or closed over.

OPTIONS is a p-list with the following possible keys:

  :TIMEOUT is a timeout duration in seconds."
  (let ((socket (gensym "SOCKET-"))
        (timeout (getf options ':timeout)))
    `(pzmq:with-socket ,socket :dealer
       (pzmq:connect ,socket ,endpoint)
       (let ((,client (make-rpc-client :socket ,socket
                                       :timeout ,timeout)))
         ,@body))))
