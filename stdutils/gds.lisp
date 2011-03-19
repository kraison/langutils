;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gds.lisp
;;;; Purpose:       Generic data structure main definitions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2005
;;;;

(in-package :stdutils.gds)

(defclass-exported standard-protocol () ())

(defmacro-exported defprotocol (name oplist &optional args)
  (with-keywords (protocol-type) args
    `(defclass-exported ,name (,(aif-ret protocol-type 'standard-protocol))
       ((operations :accessor protocol-operations :initform ',oplist)))))

;;
;; Standard familes of generic functions
;;

(defgeneric-exported add (struct key value)
  (:documentation "This function adds a keyed value to a collection"))

(defgeneric-exported drop (struct key)
  (:documentation "This function drops a keyed value from a collection"))

(defgeneric-exported contains (struct key)
  (:documentation "A predicate determining if a collection contains a keyed value"))

(defgeneric-exported get-value (struct key)
  (:documentation "The standard access for a datastructure.  Two valued return; 
                  first is actual associated value, second is whether it existed"))

(defgeneric-exported (setf get-value) (value struct key)
  (:documentation "Updating or adding a keyed value to a collection"))

(defgeneric-exported find-elt (struct value &key all key test)
  (:documentation "Finds one or more values matching value using key as an accessor and test as
                   the comparison function between value"))

(defgeneric-exported clear (struct)
  (:documentation "Reset the structure to its original state"))

(defgeneric-exported size-of (struct)
  (:documentation "The number of times you'd have to call add to reproduce this
                   structure"))

(defgeneric-exported storage-allocated (struct)
  (:documentation "The number of add operations reserved in memory for this ds"))

;; List oriented operations

(defgeneric-exported insert (struct element location )
  (:documentation ""))

;; Instance management

(defgeneric-exported copy (struct)
  (:documentation "Returns a copy in new storage of the provided structure"))

(defgeneric-exported convert (type struct)
  (:documentation "Converts one structure to another if a conversion routine exists"))

;; Iteration for data structures with a collection of elements

(defgeneric-exported map-elements (function struct)
  (:documentation "Map all the elments of the provided structure and apply function to them.
                   For tables this means all the key-value pairs as a cons pair."))

(defgeneric-exported map-keys (function struct)
  (:documentation "Maps over just the keys for keyed structures"))

(defgeneric-exported map-values (function struct)
  (:documentation "Maps over just the values for keyed structures"))

(defgeneric-exported get-iterator (struct &key type)
  (:documentation "Return a function which when called returns two values; the next element
                   in the collection and whether the iterator is completed (t on completion).
                   The value is nil when termination is reached.  The type argument is gds
                   specific and indicates what aspects of the main ds should be returned during
                   iteration"))

(defmacro-exported do-iterator ((var struct) &body body)
  (with-gensyms (iterator obj valid)
    `(let* ((,obj ,struct)
	    (,iterator (get-iterator ,obj)))
       (loop
	  (mvbind (,var ,valid) (next ,iterator)
            (if ,valid 
		(progn ,@body)
		(return)))))))

(defgeneric-exported next (iter)
  (:documentation "An function for getting the next element from a structure or iterator.
                   It takes an optional reference to an element (ie next/prev)"))

(defgeneric-exported drop-last (iterator)
  (:documentation "This drops the last visited element in a walk over a collection"))

(defgeneric-exported next-p (iter)
  (:documentation "Predicate form of next"))
