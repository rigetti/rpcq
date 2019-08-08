;;;; rpcq.asd
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

(asdf:defsystem #:rpcq
  :description "Message and RPC specifications for Rigetti Quantum Cloud Services."
  :author "Rigetti Computing <info@rigetti.com>"
  :version (:read-file-line "VERSION.txt")
  :depends-on (#:alexandria             ; Utilities
               #:parse-float            ; Float parsing
               #:yason                  ; JSON generation
               ;; RPC requirements
               #:pzmq                   ; communication layer
               #:cl-messagepack         ; message packer
               #:bordeaux-threads       ; threaded RPC server
               #:local-time             ; local time for logs
               #:uuid                   ; UUID generation
               #:cl-syslog              ; send logs to syslogd
               #:flexi-streams          ; UTF8 encode/decode
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:rpcq-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "rpcq")
               (:file "rpcq-python")
               (:file "core-messages")
               (:file "messages")
               (:file "server")
               (:file "client")))

