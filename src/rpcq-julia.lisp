;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2019 Rigetti Computing
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
;;;;
;;;; rpcq-python.lisp
;;;;
;;;; Authors: Steven Heidel
;;;;

(in-package #:rpcq)


(defparameter *julia-types*
  '(:string "String"
    :float "Float32"
    :int "Int32"
    :bool "Bool"
    :any "Any"))

(defun julia-type (field-type)
  (assert (member field-type *julia-types*)
          (field-type)
          "Unknown field-type ~S" field-type)
  (getf *julia-types* field-type))

(defun julia-field (field-spec)
  (let* ((slot-name (first field-spec))
         (field-settings (rest field-spec))
         (type (getf field-settings :type))
         (required (getf field-settings :required))
         (documentation (getf field-settings :documentation))
         (default (getf field-settings :default)))
    (list
      (format nil "\"~A\"" documentation)
      (format nil "~A::~A" slot-name (julia-type type)))))

(defun julia-struct (message-spec)
  (destructuring-bind (msg-name parent-name field-specs documentation) message-spec
    ; This absolute nonsense of a format string nicely indents the field docs
    ; and definitions
    (format nil "~
\"\"\"
~A
\"\"\"
@kwdef struct ~A
~{    ~{~A
~^    ~}~^
~}end" documentation msg-name (mapcar #'julia-field field-specs))))
