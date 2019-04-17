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
    (is (= (my-msg-required-int m)) 5)
    (is (= (hash-table-count (my-msg-optional-map m)) 1))
    (is (string= (gethash "yo" (my-msg-optional-map m)) "working"))
    (is (length (gethash "test-namespace" rpcq::*messages*)) 1)
    (is (typep (my-msg-flt m) 'double-float))
    (is (string= (my-msg-str m) "a string"))))



(deftest test-serialize-deserialize ()
  (let* ((original (make-instance 'rpcq::|RPCRequest|
                                  :|method| "test-method"
                                  :|params| (make-hash-table)
                                  :|id| "test-id"))
         (cloned (rpcq::deserialize (rpcq::serialize original))))
    (is (typep cloned 'rpcq::|RPCRequest|))
    (is (string= (rpcq::|RPCRequest-id| original)     (rpcq::|RPCRequest-id| cloned)))
    (is (string= (rpcq::|RPCRequest-method| original) (rpcq::|RPCRequest-method| cloned)))))
