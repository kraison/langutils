;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          iteration.lisp
;;;; Purpose:       Variations on do and loop iteration constructs
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

(defmacro-exported doseq ((var seq &optional (result nil)) &rest body)
  "Iterate over a sequence assigning var the value of each subsequent element"
  `(do* ((i 0 (1+ i))
	 (,var (aref ,seq 0) (aref ,seq i))
	 ((gensym) (progn ,@body)))
       ((= i (length ,seq)) ,(when result `,result))))

(defmacro-exported dolist-times ((var list times &optional (result nil)) &rest body)
  "Iterate over a list up to 'times' times"
  (let ((cnt (gensym))
	(lst (gensym)))
    `(do* ((,cnt 0 (1+ ,cnt))
	   (,lst ,list (cdr ,lst))
	   (,var (car ,list) (car ,lst)))
	 ((or (null ,lst) (= ,cnt ,times)) ,(when result `,result))
       ,@body)))

(defmacro-exported dotimes-unrolled ((var init countform blocking &optional resultform) &body body)
  "An unrolling macro that speeds up do loops by writing an unrolled version, 
   blocking determines the unrolled block size in original loop statements."
  (unless (integerp blocking)
    (error "To unroll this loop, ~S must be an integer." blocking))
  `(let ((,var ,init))
     (dotimes (ignore (floor ,countform ,blocking))
       ,@(let ((result nil))
	   (setq body (append body `((incf ,var))))
	   (dotimes (ignore blocking)
	     (setq result (nconc (copy-list body) result)))
	   result))
     (dotimes (ignore (mod ,countform ,blocking) ,resultform)
       ,@body)))				

;; 
;; Paul Grahams iteration macros
;;

(defmacro-exported while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro-exported till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro-exported for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

