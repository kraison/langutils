;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hashed-table.lisp
;;;; Purpose:       A generic table data structure; hash storage
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2005
;;;;

(in-package :stdutils.gds)

(defclass-exported hashed-table (table)
  ((table :accessor hashed-table-ref :initarg :hash :initform (hash))))

(defmethod-exported drop ((table hashed-table) key)
  (hash-rem (hashed-table-ref table) key))

(defmethod-exported get-value ((table hashed-table) key)
  (gethash key (hashed-table-ref table)))

(defmethod-exported (setf get-value) (value (table hashed-table) key)
  (setf (gethash key (hashed-table-ref table)) value))

(defmethod-exported clear ((table hashed-table))
  (hash-clr (hashed-table-ref table)))

(defmethod-exported size-of ((table hashed-table))
  (hash-table-count (hashed-table-ref table)))

(defmethod-exported storage-allocated ((table hashed-table))
  (hash-table-size (hashed-table-ref table)))


(defclass-exported hashed-table-iterator (iterator)
  ((reference :accessor reference :initarg :reference)
   (list :accessor list-ptr :initform nil)
   (last :accessor last-ptr :initform nil)
   (type :accessor iter-type :initarg :type 
	 :documentation "Type can be :key, :value or :pair")))

(defmethod-exported get-iterator ((table hashed-table) &key (type :pair))
  (make-instance 'hashed-table-iterator
		 :reference table
		 :type type))

(defmethod-exported initialize-instance :after ((table hashed-table-iterator) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (reset table))

(defmethod-exported next-p ((iter hashed-table-iterator))
  (not (null (list-ptr iter))))

(defmethod-exported next ((iter hashed-table-iterator))
  (if (next-p iter)
      (let ((acell (car (list-ptr iter))))
	(setf (list-ptr iter) (cdr (list-ptr iter)))
	(setf (last-ptr iter) acell)
	(values (extract-assoc-type acell (iter-type iter)) t))
      (progn
	(clear iter)
	(values nil nil))))

(defmethod-exported drop-last ((iter hashed-table-iterator))
  (aif (last-ptr iter)
       (progn 
	 (hash-rem (hashed-table-ref (reference iter)) (car it))
	 (setf (last-ptr iter) nil)
	 t)
       ;; conditions
       nil))

(defmethod-exported reset ((iter hashed-table-iterator))
  (setf (list-ptr iter) (hash-table->alist (hashed-table-ref (reference iter))))
  t)

(defmethod-exported clear ((iter hashed-table-iterator))
  (setf (list-ptr iter) nil)
  t)

(defun hash-table->alist (hash)
  (let ((alist nil))
    (maphash (lambda (k v)
	       (setf alist (acons k v alist)))
	     hash)
    alist))