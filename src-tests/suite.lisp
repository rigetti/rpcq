;;;; suite.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2018 Rigetti Computing
;;;;
;;;;    Licensed under the Apache License, Version 2.0 (the "License");
;;;;    you may not use this file except in compliance with the License.
;;;;    You may obtain a copy of the License at
;;;;
;;;;        http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;;    Unless required by applicable law or agreed to in writing, software
;;;;    distributed under the License is distributed on an "AS IS" BASIS,
;;;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;;    See the License for the specific language governing permissions and
;;;;    limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rpcq-tests)

(defun run-rpcq-tests (&key (verbose nil) (headless nil))
  "Run all rpcq tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':rpcq-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':rpcq-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))


(deftest test-defmessage ()
  (rpcq::defmessage my-msg ()
      ((required-int
        :type :integer
        :required t
        :documentation "Required and no default")
       (optional-map
        :type (:map :string -> :integer)
        :required nil
        :documentation "Optional mapping"
        :default (:yo "working"))
       (str
        :type :string
        :required t
        :default "a string"
        :documentation "String docs.")

       (flt
        :type :float
        :required t
        :default 0.0
        :documentation "A float."))


    :documentation "Test message")

  (let ((m (make-instance 'my-msg :required-int 5)))
    (is (= (my-msg-required-int m) 5))
    (is (= (hash-table-count (my-msg-optional-map m)) 1))
    (is (string= (gethash "yo" (my-msg-optional-map m)) "working"))
    (is (= (length (gethash "suite" rpcq::*messages*)) 1))
    (is (typep (my-msg-flt m) 'double-float))
    (is (string= (my-msg-str m) "a string"))))



(deftest test-serialize-deserialize ()
  (let* ((original (make-instance 'rpcq::|RPCRequest|
                                  :|method| "test-method"
                                  :|params| (make-hash-table)
                                  :|id| "test-with-empty-params"))
         (cloned (rpcq::deserialize (rpcq::serialize original))))
    (is (typep cloned 'rpcq::|RPCRequest|))
    (is (string= (rpcq::|RPCRequest-id| original)     (rpcq::|RPCRequest-id| cloned)))
    (is (string= (rpcq::|RPCRequest-method| original) (rpcq::|RPCRequest-method| cloned)))
    (is (= 0
           (hash-table-count (rpcq::|RPCRequest-params| original))
           (hash-table-count (rpcq::|RPCRequest-params| cloned)))))

  (let* ((original (make-instance 'rpcq::|RPCRequest|
                                  :|method| "test-method"
                                  :|params| (rpcq::prepare-rpc-call-args
                                             '("a1" a2 42 :kw1 "k1" :kw-2 "k2"))
                                  :|id| "test-with-non-empty-params"))
         (cloned (rpcq::deserialize (rpcq::serialize original)))
         (original-params (rpcq::|RPCRequest-params| original))
         (cloned-params (rpcq::|RPCRequest-params| cloned)))

    ;; We can't simply test (EQUALP ORIGINAL-PARAMS CLONED-PARAMS) here because of the 'A2 symbol in
    ;; *args, which gets deserialized as the STRING "A2".
    (is (= 3 (hash-table-count original-params) (hash-table-count cloned-params)))
    (is (equal '("a1" a2 42) (gethash "*args" original-params)))
    (is (typep (gethash "*args" cloned-params) 'vector))
    (is (equalp #("a1" "A2" 42) (gethash "*args" cloned-params)))
    (remhash "*args" original-params)
    (remhash "*args" cloned-params)
    (equalp original-params (alexandria:plist-hash-table '("kw1" "k1" "kw_2" "k2")))
    (equalp original-params cloned-params))

  (let* ((warning (make-instance 'rpcq::|RPCWarning|
                                 :|body| "The warning string."
                                 :|kind| "The type of the warning raised."))
         (original (make-instance 'rpcq::|RPCError|
                                  :|error| "The error message."
                                  :|id| "The RPC request id."
                                  :|warnings| `#(,warning)))
         (cloned (rpcq::deserialize (rpcq::serialize original))))
    (is (typep cloned 'rpcq::|RPCError|))
    (let ((cloned-warnings (rpcq::|RPCError-warnings| cloned)))
      (is (typep cloned-warnings 'vector))
      (is (= 1 (length cloned-warnings)))
      (let ((cloned-warning (elt cloned-warnings 0)))
        (is (string= (rpcq::|RPCWarning-body| warning)
                     (rpcq::|RPCWarning-body| cloned-warning)))
        (is (string= (rpcq::|RPCWarning-kind| warning)
                     (rpcq::|RPCWarning-kind| cloned-warning))))))

  (let ((rpcq::*use-false* nil))
    (is (null (rpcq:deserialize (rpcq:serialize :false))))
    (is (null (rpcq:deserialize (rpcq:serialize nil)))))

  (let ((rpcq::*use-false* t))
    (is (eql :false (rpcq:deserialize (rpcq:serialize :false))))
    (is (null (rpcq:deserialize (rpcq:serialize nil))))))
