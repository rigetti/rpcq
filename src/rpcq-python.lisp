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
;;;;
;;;; rpcq-python.lisp
;;;;
;;;; Authors: Nikolas Tezak, Eric Peterson
;;;;

(in-package #:rpcq)


(defparameter *python-types*
  '(:string "str"
    :bytes "bytes"
    :float "float"
    :integer "int"
    :bool "bool"
    :map "Dict"
    :list "List"
    :any "Any"))

(defparameter *python-instance-check-types*
  '(:string "basestring"
    :bytes "bytes"
    :float "float"
    :integer "int"
    :bool "bool"
    :map "dict"
    :list "list"
    :any "object"))


(defun python-instance-check-type (field-type)
  (let ((*python-types* *python-instance-check-types*))
    (python-type field-type)))

(defun python-argspec-default (field-type default &optional defaultp)
  "Translate DEFAULT values for immutable objects of a given FIELD-TYPE to python.

DEFAULTP indicates whether DEFAULT has a value of NIL because NIL was provided in the DEFMESSAGE (T), or because it was missing in the DEFMESSAGE (NIL)."
  (typecase field-type
    ((eql :string)
     (if default
         (format nil "~S" default)
         "None"))
    ((eql :bytes)
     (if default
         (format nil "b~S" (to-string default))
         "None"))
    ((eql :bool)
     (cond
       ((not defaultp) "None")
       (default        "True")
       (t              "False")))
    ((eql :integer)
     (if default
         (format nil "~d" default)
         "None"))
    ((eql :float)
     (if default
         (format nil "~e" default)
         "None"))
    ((cons (eql :list))
     (if default
         (format nil "[~{~S~^, ~}]" default)
         "field(default_factory=list)"))
    ((cons (eql :map))
     (when default
       (warn "I don't know how to encode default dictionaries into generated python3 output."))
     "field(default_factory=dict)")
    (otherwise
     "None")))

(defun python-type (field-type)
  "Always return a basic python type not List[...] or Dict[...] for
instance checks."
  (python-typing-type
   (if (listp field-type)
       (car field-type)
       field-type)))

(defun python-typing-type (field-type)
  "Return the python typing-module compliant field type"
  (etypecase field-type
    (keyword
     (assert (member field-type *python-types*)
             (field-type)
             "Unknown field-type ~S" field-type)
     (getf *python-types* field-type))
    (symbol
     ;; field-type is assumed to be message object
     (format nil "~a" field-type))
    ((cons (eql :list))
     (format nil "List[~a]" (python-typing-type (cadr field-type))))
    ((cons (eql :map))
     (assert (string= (symbol-name (caddr field-type)) "->")
             (field-type)
             "Bad mapping spec.")
     (format nil
             "Dict[~a, ~a]"
             (python-typing-type (cadr field-type))
             (python-typing-type (cadddr field-type))))))

(defun python-maybe-optional-typing-type (field-type required)
  "Rerturn the python type string for FIELD-TYPE while
accounting for whether the field is REQUIRED.
"
  (let ((b (python-typing-type field-type)))
    (if (or required (listp field-type))
        b
        (format nil "Optional[~a]" b))))

(defun python-collections-initform (field-type default)
  "Translate a DEFAULT value of type FIELD-TYPE to a python initform."
  (check-type field-type list)
  (etypecase field-type
    ;; handle lists
    ((cons (eql :list))
     (if (null default)
         "[]"
         (with-output-to-string (s)
           (yason:encode default s))))

    ;; handle mappings
    ((cons (eql :map))
     (if (null default)
         "{}"
         (with-output-to-string (s)
           (yason:encode (%plist-to-string-hash-table default) s))))))


(defun python-message-spec (stream messages &optional parent-modules)
  "Print an importable python file with the message definitions."
  (flet ((python-out (line-list)
           (dolist (line line-list)
             (apply 'format stream line)
             (terpri stream))
           (terpri stream)))
    (format stream "~
#!/usr/bin/env python

\"\"\"
WARNING: This file is auto-generated, do not edit by hand. See README.md.
\"\"\"

import sys

from warnings import warn
from rpcq._base import Message
from typing import Any, List, Dict, Optional

if sys.version_info < (3, 7):
    from rpcq.external.dataclasses import dataclass, field, InitVar
else:
    from dataclasses import dataclass, field, InitVar~%~%")
    (format stream "~{from ~a import *~%~}~%" parent-modules)

    (dolist (message-spec messages)
      (destructuring-bind (msg-name parent-name field-specs documentation) message-spec

        ;; print the class header
        (python-out `(("@dataclass(eq=False, repr=False)")
                      ("class ~a(~a):"                     ,(symbol-name msg-name)
                                                           ,(if parent-name
                                                                (symbol-name parent-name)
                                                                "Message"))
                      ("    \"\"\"")
                      ("    ~a"                            ,documentation)
                      ("    \"\"\"")))

        (let ((deprecated-fields nil))

          ;; python dataclasses require their fields to be written in the order
          ;; * required slots
          ;; * optional slots
          ;; * deprecated slots

          (labels ((requiredp (r)
                     (and (not (member ':default (rest r)))
                          (getf (rest r) ':required)))
                   (optionalp (r)
                     (not (requiredp r)))
                   (deprecatedp (r)
                     (or (getf (cdr r) ':deprecated)
                         (getf (cdr r) ':deprecated-by))))
            (setf field-specs (sort (copy-seq field-specs)
                                    (lambda (r s)
                                      (or (and (requiredp r) (optionalp s))
                                          (and (requiredp r) (deprecatedp s))
                                          (and (optionalp r) (deprecatedp s)))))))

          (dolist (field-spec field-specs)
            (let* ((slot-name (car field-spec))
                   (field-settings (cdr field-spec))
                   (type (getf field-settings ':type))
                   (required (getf field-settings ':required))
                   (documentation (getf field-settings ':documentation))
                   (defaultp (member ':default field-settings))
                   (default (getf field-settings ':default))
                   (deprecated (getf field-settings ':deprecated))
                   (deprecates (getf field-settings ':deprecates))
                   (deprecated-by (getf field-settings ':deprecated-by)))
              ;; optional fields automatically acquire a NIL default
              (unless (or required defaultp)
                (setf default nil)
                (setf defaultp t))

              ;; print the slot descriptor
              (cond
                ;; recipe for a deprecated slot
                ((or deprecated-by deprecated)
                 (python-out `(("    ~a: InitVar[~a] = None" ,(symbol-name slot-name)
                                                             ,(python-maybe-optional-typing-type type required))
                               ("    \"\"\"~a\"\"\""         ,documentation)))
                 (when deprecated
                   (push (list slot-name nil required "None") deprecated-fields)))
                ;; recipe for a deprecating slot
                (deprecates
                 (let ((definite-default (python-argspec-default type default (member ':default field-settings))))
                   (python-out `(("    ~a: ~a = ~a"      ,(symbol-name slot-name)
                                                         ,(python-maybe-optional-typing-type type t)
                                                         ,definite-default)
                                 ("    \"\"\"~a\"\"\""   ,documentation)))
                   (push (list deprecates slot-name required definite-default) deprecated-fields)))
                ;; recipe for a slot with a default value
                (defaultp
                 (python-out `(("    ~a: ~a = ~a"       ,(symbol-name slot-name)
                                                        ,(python-maybe-optional-typing-type type required)
                                                        ,(python-argspec-default type default
                                                                                 (member ':default field-settings)))
                               ("    \"\"\"~a\"\"\""    ,documentation))))
                ;; recipe for a slot otherwise
                (t
                 (python-out `(("    ~a: ~a"           ,(symbol-name slot-name)
                                                       ,(python-maybe-optional-typing-type type required))
                               ("    \"\"\"~a\"\"\""   ,documentation)))))))

          ;; deprecated fields need special care.
          (when deprecated-fields
            ;; (1) add fake getters / setters
            (dolist (field-spec deprecated-fields)
              (destructuring-bind (old new new-required default-value) field-spec
                (declare (ignore new-required default-value))
                (when new
                  (python-out `(("    @property")
                                ("    def ~a(self):"                                ,(symbol-name old))
                                ("        warn('~a is deprecated, use ~a instead')" ,(symbol-name old)
                                                                                    ,(symbol-name new))
                                ("        return self.~a"                           ,(symbol-name new))
                                ("")
                                ("    @~a.setter"                                   ,(symbol-name old))
                                ("    def ~a(self, value):"                         ,(symbol-name old))
                                ("        warn('~a is deprecated, use ~a instead')" ,(symbol-name old)
                                                                                    ,(symbol-name new))
                                ("        self.~a = value"                          ,(symbol-name new)))))))

            ;; (2) add extra fields to output
            (python-out `(("    def _extend_by_deprecated_fields(self, d):")
                          ("        super()._extend_by_deprecated_fields(d)")))
            (dolist (field-spec deprecated-fields)
              (destructuring-bind (old new new-required default-value) field-spec
                (declare (ignore new-required default-value))
                (when new
                  (python-out `(("        d.~a = d.~a" ,(symbol-name old)
                                                       ,(symbol-name new)))))))

            ;; (3) tolerate extra fields on input
            (format stream
                    "    def __post_init__(self, ~{~a~^, ~}):~%"
                    (mapcar (alexandria:compose #'symbol-name #'first) deprecated-fields))
            (dolist (field-spec deprecated-fields)
              (destructuring-bind (old new new-required default-value) field-spec
                (let* ((appearance-index (search "field" default-value))
                       (default-property-p (and appearance-index (zerop appearance-index))))
                  (cond
                    (new
                     (python-out `(,(if default-property-p
                                        `("        if not isinstance(~a, property):"                 ,(symbol-name old))
                                        `("        if ~a is not ~a:"                                 ,(symbol-name old)
                                                                                                     ,default-value))
                                   ("            if \"~a\" not in self.__dict__ or self.~a is None:" ,(symbol-name new)
                                                                                                     ,(symbol-name new))
                                   ("                warn('~a is deprecated, use ~a instead')"       ,(symbol-name old)
                                                                                                     ,(symbol-name new))
                                   ("                self.__dict__[\"~a\"] = ~a~%"                   ,(symbol-name new)
                                                                                                     ,(symbol-name old))))
                     (when new-required
                       (python-out `(("        if \"~a\" not in self.__dict__ or self.~a is None:" ,(symbol-name new)
                                                                                                   ,(symbol-name new))
                                     ("            raise(TypeError(\"~a is a required key.\"))~%"  ,(symbol-name new))))))
                    (t
                     (python-out `(,(if default-property-p
                                        `("        if not isinstance(~a, property):"                      ,(symbol-name old))
                                        `("        if ~a is not ~a:"                                      ,(symbol-name old)
                                                                                                          ,default-value))
                                   ("            warn('~a is deprecated; please don\\'t set it anymore')" ,(symbol-name old)))))))))))
        (python-out '())))))
