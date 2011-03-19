;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          assoc-table.lisp
;;;; Purpose:       A generic data structure; assoc-list tables
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2005
;;;;

(in-package :stdutils.gds)

(defclass-exported assoc-table (table)
  ((alist :accessor alist :initarg :alist :initform nil)
   (key-test :accessor key-test :initarg :key-test :initform #'eq)))

(defmethod-exported drop ((table assoc-table) key)
  (assoc-del key (alist table)))

(defmethod-exported get-value ((table assoc-table) key)
  (assoc-get (alist table) key))

(defmethod-exported (setf get-value) (value (table assoc-table) key)
  (assoc-setf (alist table) key value)
  value)

(defmethod-exported find-value ((table assoc-table) value &key all (key #'identity) (test #'eq))
  (if all
      (let ((list nil))
	(map-elements
	 (lambda (pair)
	   (format t "~A~%" pair)
	   (when (funcall test value (funcall key (cdr pair)))
	     (push pair list)))
	 table)
	list)
      (find value (alist table) :key (lambda (pair) (funcall key (cdr pair))) :test test)))

(defmethod-exported clear ((table assoc-table))
  (setf (alist table) nil))

(defmethod-exported size-of ((table assoc-table))
  (length (alist table)))

(defclass-exported assoc-table-iterator (iterator)
  ((current :accessor current-ptr :initarg :current)
   (reference :accessor reference :initarg :reference)
   (last :accessor last-ptr :initform nil)
   (type :accessor iter-type :initarg :type 
	 :documentation "Type can be :key, :value or :pair")))

(defmethod-exported get-iterator ((table assoc-table) &key (type :pair))
  (make-instance 'assoc-table-iterator
		 :reference table
		 :current (alist table)
		 :type type))

(defmethod-exported next-p ((iter assoc-table-iterator))
  (not (null (current-ptr iter))))

(defun extract-assoc-type (acell type)
  (ecase type
    (:key (car acell))
    (:value (cdr acell))
    (:pair acell)))

(defmethod-exported next ((iter assoc-table-iterator))
  (if (next-p iter)
      (let ((acell (car (current-ptr iter))))
	(setf (current-ptr iter) (cdr (current-ptr iter)))
	(setf (last-ptr iter) acell)
	(let ((value (extract-assoc-type acell (iter-type iter))))
	  (values value t)))
      (progn
	(clear iter)
	(values nil nil))))

(defmethod-exported drop-last ((iter assoc-table-iterator))
  (aif (last-ptr iter)
       (progn
	 (drop (reference iter) (car it))
	 (setf (last-ptr iter) nil)
	 t)
       nil))

(defmethod-exported reset ((iter assoc-table-iterator))
  (setf (current-ptr iter) (alist (reference iter)))
  t)

(defmethod-exported clear ((iter assoc-table-iterator))
  "To ensure that things are released for GC"
  (setf (current-ptr iter) nil)
  t)
  
    
  
