;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          datafile
;;;; Purpose:       Simple system for optimizing performance of datafiles
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :utils)

(defclass datafile ()
  ((initfunc
    :accessor datafile-init-func
    :initarg :initfunc
    :initform #'identity)
   (paths
    :accessor datafile-filenames
    :initarg :paths)
   (initargs 
    :accessor datafile-init-args
    :initarg :initargs
    :initform nil)
   (instance
    :accessor instance
    :initform nil)
   (initialized
    :type boolean))
  (:documentation
   "This is a class to use for simple databases
    and other structures that are built from 
    files or computed at some initial time.
    Can be a container or a base class."))

(defmethod initialize-data ((df datafile) &rest args)
  (with-slots (initialized instance initfunc initargs paths) df
      (setf instance (apply initfunc (cons paths (aif args it initargs)))
	    initialized t)))
  
(defmethod ensure-data ((df datafile))
  (with-slots (initialized) df
    (unless initialized
      (initialize-data df))))
  
(defmethod clear-data ((df datafile))
  (with-slots (initialized instance) df
    (allf nil instance initialized)))


(defmacro def-datafile (name initfunc files &optional initargs)
  `(defvar ,name 
     (make-instance 'datafile 
		    :initfunc ,initfunc
		    :paths (mapcar #'pathname (mklist ,files))
		    :initargs ,initargs)))
   
;;;;
;;;; Nice clean way to aggregate datafiles
;;;;

;; want to be able to define a bunch of files at the same time
