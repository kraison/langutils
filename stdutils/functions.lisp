;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Purpose:       This file contains utilities related to function creation,
;;;;                variants on functions, higher order composition and creation
;;;;                and such.
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

;; --------------------------------
;; Define self recursive functions
;; without interior functions

(defmacro-exported defun-rec (name parms &body body)
  `(defun ,name ,parms
     (labels ((,name ,parms ,@body))
       (funcall #',name ,@parms))))


#+duplicates
(defmacro-exported alambda (parms &body body)
  "Capture a variable self in the body of
   program that allows the program to recurse
   on itself"
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro-exported ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
			(case (length args)
			  (0 nil)
			  (1 (car args))
			  (t `(let ((it , (car args)))
				, (self (cdr args))))))
	       args)))

;; -------------------------------
;; Construct functions & calls

(defmacro fn (expr) `#',(rbuild expr))
 
(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns)) 
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

;; ---------------------------------
;; Function composition and control
;; composition


(defun-exported compose (&rest fns)
  "Function composition."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))
  

(defun-exported fn-compose (&rest fns)
  (apply #'compose fns))


(defun-exported fn-sequence (&rest fns)
  "Function sequencing with shared args"
  #'(lambda (&rest args)
      (loop for fn in fns do
	(apply fn args))))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x) 
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;; NOTE: Include pg's macro for composition, much cleaner

(defun-exported curry (fn arg)
  #'(lambda (&rest args)
      (apply fn (cons arg args))))

(defmacro-exported ncurry (fn &rest def-args)
  `(lambda (&rest dyn-args)
     (apply ,fn ,@def-args dyn-args)))

(defmacro-exported vcurry (fn pattern &rest orig-args)
  "Pattern determines the calling arglist over fn
   where nils are replaced with arguments to the
   new lambda in order and vars are replaced with
   the arguments in args in order"
  (let* ((offsets (collect-offsets pattern))
	 (diff (- (length offsets) (length pattern))))
    (flet ((compute-args (&aux arglist (argnum 0))
	     (loop for index from 0 upto (1- (length pattern)) do
	       (cond ((find index offsets)
		      (push (pop orig-args) arglist))
		     (t (push `(nth ,argnum args) arglist)
			(incf argnum))))
	     (nreverse arglist)))
      `#'(lambda (&rest args)
	   (assert (>= (length args) ,diff))
	   (funcall ,fn ,@(compute-args))))))

(defun-exported curry2 (fn arg)
  #'(lambda (arg1 &rest args)
      (apply fn arg1 arg args)))

;; --------------------------------
;; Cheap memoization

(defun-exported memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache) 
                    (apply fn args)))))))

(defmacro-exported defun-memo (fn args &body body)
  "Define a memoized function"
  `(memoize (defun ,fn ,args . ,body)))

(defun-exported memoize1 (fn)
  (let ((cache nil)
	(val nil))
    #'(lambda (&rest args)
	(if (equal args cache)
	    val
	  (progn
	    (setf cache args)
	    (setf val (apply fn args)))))))

(defmacro-exported defun-memo1 (fn args &body body)
  "Define a memoized function"
  `(memoize1 (defun ,fn ,args . ,body)))


;; -------------------------------
;; Destructuring 

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
       (if rest
           `((,rest (subseq ,seq ,n)))
           (let ((p (car pat))
                 (rec (destruc (cdr pat) seq atom? (1+ n))))
             (if (funcall atom? p)
                 (cons `(,p (elt ,seq ,n))
                       rec)
                 (let ((var (gensym)))
                   (cons (cons `(,var (elt ,seq ,n))
                               (destruc p var atom?))
                         rec))))))))


