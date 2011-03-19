;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          shorthand.lisp
;;;; Purpose:       Abbreviations and other shortcuts; symbol manipulation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

;; =====================
;; Author: Ian Eslick
;; 
;; With thanks to Paul Graham

(in-package :stdutils)

(defmacro-exported abbrev (short long)
  "Create an abbreviation for an existing function or macro via macro instantiation"
  `(defmacro ,short (&body args)
     `(,',long ,@args)))

(defmacro-exported abbrevs (&body names)
  "Map abbrev over a list of name pairs: (ab nam ab nam ...)"
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))

;; Standard abbreviations we use often

(abbrevs mvsetq multiple-value-setq)

;; Do these manually for cool slime indenting
(defmacro mvbind (binds call &body body)
  `(multiple-value-bind ,binds ,call 
     (declare (ignorable ,@binds))
     ,@body))

(defmacro dbind (binds call &body body)
  `(destructuring-bind ,binds ,call ,@body))

(eval-when (eval compile load)
  (export 'dbind)
  (export 'mvbind)
  (export 'mvsetq))

;; Shorthand for making symbols
(defun-exported mksymbol (&rest component-names)
  (labels ((to-string (x)
	     (cond ((symbolp x) 
		    (symbol-name x))
		   ((stringp x)
		    x))))
    ;; Convert names to concatenated string and intern the symbol
    (intern 
     (apply #'concatenate 
	    (cons 'string 
		  (mapcar #'to-string component-names))))))

(defun-exported mkupsymbol (&rest component-names)
  (apply #'mksymbol (mapcar #'string-upcase component-names)))

(defun-exported mkkeysym (text)
  (intern (string-upcase text) 'keyword))

(defun-exported gensyms-ignorable (symlist)
  "Generates ignorable declarations for a list of gensyms"
  (labels ((iter (ilist symlist)
	     (if (null symlist) 
		 ilist
	       (iter (cons `(ignorable ,(car symlist)) ilist)
		     (cdr symlist)))))
    `(declare ,@(iter nil symlist))))

(defun-exported gensyms (num &key (last (gensym)))
  "Returns a list of 'num' gensysms"
  (nconc (map0-n #'(lambda (x) 
		     (declare (ignore x))
		     (gensym)) 
		 (- num 2)) ;; account for 0-n
	 (list last)))

(defmacro-exported mvretn (num expr)
  "Returning the num'th value from a multi
   value returning expression, expr"
  (let* ((sym (gensym))
	 (syms (gensyms num :last sym)))
    `(mvbind ,syms ,expr ,(gensyms-ignorable syms) ,sym)))

;; --------------------------
;; Other destructuring binds
;; --------------------------

(defmacro-exported alist-bind (names alist &body body)
  "Bind names to associated values in assoc-list or nil"
  `(let (,@(mapcar #'(lambda (x) 
		       (list x `(aif (or (assoc ',x ,alist)
					 (assoc (intern (symbol-name ',x) (find-package :keyword)) ,alist))
				     (cdr it)
				     nil)))
		   names))
     ,@body))



