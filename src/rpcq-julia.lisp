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
    :integer "Int32"
    :bool "Bool"
    :map "Dict"
    :list "Array"
    :any "Any"))

(defun julia-type (field-type)
  (etypecase field-type
    (keyword
     (assert (member field-type *julia-types*)
             (field-type)
             "Unknown field-type ~S" field-type)
     (getf *julia-types* field-type))
    (symbol
     (format nil "~A" field-type))
    ((cons (eql :list))
     (format nil "Array{~A}" (julia-type (second field-type))))
    ((cons (eql :map))
     (assert (string= (symbol-name (third field-type)) "->")
             (field-type)
             "Bad mapping spec.")
     (format nil
             "Dict{~A, ~A}"
             (julia-type (second field-type))
             (julia-type (fourth field-type))))))

(defun julia-optional-type (field-type required)
  (let ((type (julia-type field-type)))
    (if (or required (listp field-type))
      type
      (format nil "Union{~A, Nothing}" type))))

(defun julia-default (field-type default)
  (etypecase field-type
    (keyword
      (if (and (null default) (not (eql field-type :bool)))
        "nothing"
        (case field-type
          (:string (format nil "~S" default))
          (:bytes (format nil "b~S" (to-string default)))
          (:bool (if default "true" "false"))
          (:integer (format nil "~D" default))
          (:float (format nil "~E" default)))))
    ((cons (eql :list)) "[]")
    ((cons (eql :map)) "Dict()")
    (t (if (null default) "nothing"))))

(defun julia-field (field-spec)
  (let* ((slot-name (first field-spec))
         (field-settings (rest field-spec))
         (type (getf field-settings :type))
         (required (getf field-settings :required))
         (documentation (getf field-settings :documentation))
         (default (getf field-settings :default))
         (defaultp (member :default field-settings)))
    (list
      (format nil "\"~A\"" documentation)
      (format nil "~A::~A~@[ = ~A~]"
        slot-name
        (julia-optional-type type required)
        (if (or (not required) defaultp) (julia-default type default))))))

(defun julia-struct (message-spec)
  (destructuring-bind (msg-name parent-name field-specs documentation) message-spec
    ; This absolute nonsense of a format string nicely indents the field docs
    ; and definitions
    (format nil "~
\"\"\"
~A
\"\"\"
Base.@kwdef struct ~A
~{    ~{~A~^
    ~}~^

~}
end" documentation msg-name (mapcar #'julia-field field-specs))))

(defun julia-messages ()
  (with-open-file (f "messages.jl"
                   :direction ':output
                   :if-exists ':supersede)
    (format f "~{~A~^~%~%~}" (mapcar #'julia-struct *messages*))))
