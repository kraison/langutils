;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vechash.lisp
;;;; Purpose:       Fast vector element hash implementation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils.gds)

(defprotocol table
    (;; element ops
     add drop get-value (setf get-value) 
     find-value contains
     ;; global ops
     clear empty 
     copy convert 
     size-of storage-allocated copy convert 
     table-elements table-keys table-values
     ;; iteration 
     map-elements map-keys map-values get-iterator))

(defmethod-exported add ((table table) key value)
  (setf (get-value table key) value))

(defmethod-exported contains ((table table) key)
  (awhen2 (get-value table key) t))

(defmethod-exported empty ((table table))
  (= 0 (size-of table)))

(defmethod-exported storage-allocated ((table table))
  (size-of table))

(defmacro-exported do-table ((key value table) &body body)
  (with-gensyms (pair)
    `(do-iterator (,pair ,table)
       (dbind (,key . ,value) ,pair
	      (declare (ignorable ,key ,value))
	      (progn ,@body)))))

(defmethod-exported copy ((table table))
  "Default table copy.  Can override with something more efficient
   for implementations that are amenable to such optimization"
  (let ((new (make-instance (type-of table))))
    (do-table (key value table)
      (add table key value))
    new))

(defmethod-exported map-elements (function (table table))
  (do-table (key value table)
    (funcall function key value)))

(defmethod-exported map-keys (function (table table))
  (do-table (key value table)
    (funcall function key)))

(defmethod-exported map-values (function (table table))
  (do-table (key value table)
    (funcall function value)))

(defmethod-exported table-elements ((table table))
  (let ((list nil))
    (do-iterator (pair table)
      (push pair list))
    (nreverse list)))

(defmethod-exported table-keys ((table table))
  (let ((list nil))
    (map-keys (lambda (key) (push key list)) table)
    (nreverse list)))

(defmethod-exported table-values ((table table))
  (let ((list nil))
    (map-values (lambda (value) (push value list)) table)
    (nreverse list)))
  
(defmethod-exported convert ((source table) type)
  "Default implementation of convert for tables"
  (let ((inst (make-instance type)))
    (map-elements (lambda (key value) 
		    (add inst key value))
		  source)
    inst))

(defmethod-exported find-elt ((table table) value &key all (key #'identity) (test #'eq) &aux list)
  (do-iterator (pair table)
    (format t "~A,~A~%" (car pair) (cdr pair))
    (when (funcall test (funcall key (cdr pair)) value)
      (if all
	  (push pair list)
	  (return-from find-elt pair))))
  (nreverse list))
