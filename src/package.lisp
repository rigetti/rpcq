;;;; src/package.lisp
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

(defpackage #:rpcq
  (:use #:cl #:alexandria #:yason)
  (:export #:defmessage
           #:serialize
           #:deserialize
           #:clear-messages
           #:python-message-spec
           ;; RPC client/server functions
           #:make-dispatch-table
           #:dispatch-table-add-handler
           #:start-server
           #:with-rpc-client
           #:with-unique-rpc-address
           #:rpc-call
           ;; RPC client/server errors and error accessors
           #:not-an-rpcrequest
           #:not-an-rpcrequest-object
           #:unknown-rpc-method
           #:unknown-rpc-method-name
           #:rpc-error
           #:rpc-error-string
           #:rpc-error-request-id
           #:rpc-protocol-error
           #:rpc-protocol-error-id
           #:rpc-protocol-error-object
           )
  (:shadow #:phase #:stream #:time #:type #:length #:method #:error #:sequence))
