;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          map.lisp
;;;; Purpose:       Extensions and variations of the map operator
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

(eval-when (eval compile load)
  (proclaim '(inline last1 single append1 conc1))
  (proclaim '(optimize speed (safety 1) (debug 1))))

(defmacro-exported map-> (fn start test-fn succ-fn)
  "A general map function to assemble a map operation over arbitrary sequences. 
   It takes an initial value 'start', a completion test function and a successor
   function which computes the next value"
  (let ((i (gensym))
	(result (gensym)))
    `(do ((,i ,start (funcall ,succ-fn ,i))
	  (,result nil))
	 ((funcall ,test-fn ,i) (nreverse ,result))
       (push (funcall ,fn ,i) ,result))))

(defmacro-exported mapa-b (fn a b &optional (step 1))
  "map over the integers from a to b"
  `(map-> ,fn ,a 
	  #'(lambda (x) (> x ,b))
	  #'(lambda (x) (+ x ,step))))

(defmacro-exported map0-n (fn n)
  "map over the integers from 0 to n"
  `(mapa-b ,fn 0 ,n))

(defun-exported collect (fn list)
  "cons together non-nil results"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((rec (values list)
		(cond ((null list)
		       (nreverse values))
		      (t (rec (aif (funcall fn (car list))
				   (cons it values)
				   values)
			      (cdr list))))))
    (rec nil list)))

(defun-exported collectn (fn list &rest lists)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((values nil))
    (loop for elt in list 
	  until (some #'null lists) do
	  (awhen (apply fn (cons elt (mapcar #'car lists)))
		 (setf values (cons it values)))
	  (setf lists (mapcar #'cdr lists)))
    (nreverse values)))

(defun-exported last1 (lst)
  (car (last lst)))

(defun-exported single (lst)
  (and (consp lst) (not (cdr lst))))

(defun-exported append1 (lst obj) 
  (append lst (list obj)))

(defun-exported conc1 (lst obj)   
  (nconc lst (list obj)))

(defun-exported mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun-exported mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun-exported rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
             #'(lambda (&rest args) 
                 (apply #'rmapcar fn args))
             args)))

(defun-exported cars (list)
  (mapcar #'car list))

(defun-exported cdrs (list)
  (mapcar #'cdr list))

(defun-exported cddrs (list)
  (mapcar #'cddr list))

(defun-exported mapcar-idx (fn list)
  (loop for elt in list 
        for i from 0 collecting
	(funcall fn elt i)))

(defmacro-exported map-across (fn array)
  (with-gensyms (elt)
    `(loop for ,elt across ,array collecting
	   (funcall ,fn ,elt))))

(defmacro-exported map-across-n (fn array n)
  (with-gensyms (elt idx)
    `(loop for ,elt across ,array 
           for ,idx from 0 upto (1- ,n) do
	   (funcall ,fn ,elt))))

(defmacro-exported map-across-idx (fn array)
  (with-gensyms (elt idx arry)
    `(let ((,arry ,array))
      (loop for ,elt across ,arry 
       for ,idx from 0 upto (1- (length ,arry)) do
       (funcall ,fn ,elt ,idx)))))

;;
;; Accumulation
;; 

(defmacro-exported accumulate-init (fn init &rest lists)
  `(accum ,fn ,init ,@lists))

(defmacro-exported accumulate-int (fn &rest lists)
  `(accum ,fn 0 ,@lists))

(defmacro-exported accumulate (fn &rest lists)
  `(accum ,fn nil ,@lists))

(defun-exported accum (fn init &rest lists)
  (labels ((cdrs (args) (mapcar #'cdr args))
           (cars (args) (mapcar #'car args))
	   (rec (val args)
		(if (or (null args)
			(and (consp args) (null (car args))))
		    val
		  (rec (apply fn val (cars args))
		       (cdrs args)))))
    (rec init lists)))

(defun-exported accumulate-list (fn list)
  "Map a binary operation over a list, accumulating
   the incremental results fn(fn(1,2),3)..."
  (if (= 1 (length list))
      (car list)
    (accumulate-init fn (car list) (cdr list))))

(defmacro-exported sum-list (list)
  `(apply #'+ ,list))

(defmacro-exported prod-list (list)
  `(apply #'* ,list))

