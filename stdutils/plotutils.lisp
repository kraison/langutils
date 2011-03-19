;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          plotutils.lisp
;;;; Purpose:       Extensions to cllib's interface
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  January 2005

(in-package :utils)

;; -------------------
;; General plot utils
;; -------------------

(defun-exported add-titles-to-data (titles datums)
  (assert (>= (length titles) (length datums)))
  (mapcar (lambda (title dlist)
	    (cons title dlist))
	  titles
	  datums))


;; ----------------------------
;; Data manipulation utilities
;; ----------------------------

(defun find-all-classes (list class-fn &key key)
  (labels ((get-element-class (element) 
			      (funcall class-fn 
				       (if key (funcall key element) element)))
	   (rec (rlist classes)
		(if (null rlist)
		    classes
		  (let ((candidate (get-element-class (car rlist))))
		    (if (find candidate classes)
			(rec (cdr rlist) classes)
		      (rec (cdr rlist) (cons candidate classes)))))))
    (rec list nil)))
  

;; -------------------------
;; Top level plot functions
;; -------------------------

(defun-exported compute-multiple-class-histogram (list class-fn 
					      &rest opts &key (num-bins nil) (key nil) (classes nil) (normalize nil)
					      &allow-other-keys &aux (bsize nil) (clss nil))
  "Classifies and plots data elements in the list according to the
   value returned by class-fn when applied to an element of the list.  
   Accepts plot keyword options"
  (labels ((num-bins () (if num-bins num-bins 20))
	   (total-size () (length list))
	   (bin-size () (if bsize bsize (setq bsize (ceiling (total-size) (num-bins)))))
	   (get-classes () 
			(if classes classes
			     (if clss clss
			       (setq clss (find-all-classes list class-fn :key key)))))
	   (make-bins () 
		      (repeat-fn (lambda () 
				   (mapcar (lambda (class)
					     (cons class 0))
					   (get-classes)))
				 (num-bins)))
	   (get-series (class bins)
		       (mapcar (lambda (bin)
				 (let ((count (cdr (assoc class bin))))
;;				   (format t "count: ~A norm: ~A~%" count norm)
				   (if normalize
				       (ceiling count (bin-size))
				     count)))
			       bins))
	   (get-element-class (element) 
		      (funcall class-fn 
			       (if key (funcall key element) element))))
    (let ((bins (make-bins)))
      (loop 
	with bin = 0
	for count from 0
	for elt in list do
	(aif (assoc (get-element-class elt) 
		    (nth bin bins))
	     (incf (cdr it)))
	(if (= 0 (mod count (bin-size))) (incf bin)))
      (let ((plist (mapcar (lambda (class)
			     (cons class (get-series class bins)))
			   (get-classes))))
	(apply #'cllib:plot-lists 
	       `(,plist 
		 ,@(rem-keywords opts '(:num-bins :key :classes :normalize))
		 :xlabel "Counts"))))))

      
	   