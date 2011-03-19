;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          random.lisp
;;;; Purpose:       Random number functions
;;;; Programmer:    Ian S Eslick
;;;; Date Started:  October 2004
;;;;
;;;; *************************************************************************

(in-package #:stdutils)

(defun-exported seed-random-generator ()
  "Evaluate a random number of items"
  (let ((randfile (make-pathname 
		   :directory '(:absolute "dev") 
		   :name "urandom")))
    (setf *random-state* (make-random-state t))
    (if (probe-file randfile)
	(with-open-file
	    (rfs randfile :element-type 'unsigned-byte)
	  (let* 
	      ;; ((seed (char-code (read-char rfs))))
	      ((seed (read-byte rfs)))
	    ;;(format t "Randomizing!~%")
	    (loop
		for item from 1 to seed
		do (loop
		       for it from 0 to (+ (read-byte rfs) 5)
		       do (random 65536))))))))

(defun-exported random-integer-list (lower upper n)
  (labels ((make-int-list (list count)
	     (if (>= count n)
		 list
	       (make-int-list (cons (+ (random (- upper lower)) lower) list)
			      (1+ count)))))
    (make-int-list nil 0)))

(defun-exported random-fraction-list (precision n)
  (labels ((make-fraction-list (list count)
	     (if (>= count n)
		 list
	       (make-fraction-list (cons (/ (random precision) (coerce precision 'float)) list)
				   (1+ count)))))
    (make-fraction-list nil 0)))

(defun-exported random-order (list)
  "Return a new list in a random order;
   This may not be 'truely' random"
  (let ((source (copy-list list))
        (result nil))
    (loop for i from (length list) downto 1 do
	(push (nth (random i) source) result)
	(setf source (delete (car result) source)))
    result))

(defun-exported random-element (list)
  "Choose a random element from the list and return it"
  (nth (random (length list)) list))

(defmacro-exported random-choice (&rest exprs)
  "A random choice from a set of expressions"
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defun-exported random-subset (list size)
  "Extract a random subset by percent (size < 1)
   and return two values, the subset and the remainder 
   set - order is not maintained"
  (let ((offset (ceiling (if (> size 1)
			     size
			     (* size (length list)))))
	(rlist (random-order list)))
    (values (subseq rlist 0 offset)
	    (subseq rlist offset))))

(defun random-choice-dist (list dist)
  "A random choice from list using the enumerated
   probability distribution in dist"
  (assert (eq (length list) (length dist)))
  (assert (= 1.0 (accumulate-int #'+ dist)))
  
  ;; Simple way to compute over distribution
  ;; create a sample from one to 100k (resolution
  ;; to .00001 and map into range described by
  ;; dist
  (nth (position (/ (random 100000) 100000) (create-range dist) :test #'<)
       list))

(defun create-range (dist &optional (prior-sum 0))
  (if (null dist)
      nil
    (let ((sum (+ prior-sum (car dist))))
      (cons sum
	    (create-range (cdr dist) sum)))))
	  

(defun sampled-dist (fn iters &optional values)
  "Compute an estimated probability over equal comparable
   values from successive no-arg calls to fn over
   iters.  Returns of an alist of (value . prob)"
  (let ((list (mapcar #'(lambda (x) (cons x 0)) values)))
    (dotimes (i iters)
      (let ((observed (funcall fn)))
	(aif (assoc observed list)
	     (incf (cdr it))
	     (push (cons observed 1) list))))
    (mapcar #'(lambda (rec) (setf (cdr rec) (/ (cdr rec) iters)))
	    list)))