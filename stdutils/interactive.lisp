;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          interactive.lisp
;;;; Purpose:       Stuff you might want to do at the repl as you are programming or exploring
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

;; ====================================
;; Interactive utilities:
;; ====================================

(in-package :stdutils)

;; -------------------------------
;; asdf command line utils
;;

(defun-exported asdf-load (name &key force)
  (asdf:operate 'asdf:load-op name :force force))

;;(defun-exported asdf-init (name &key force)
;;  (asdf:operate 'asdf-config:initialize-op name :force force))

;; -------------------------------
;; A nickname for cl-ppcre portable regex
;; 

(defpackage #:cl-ppcre
    (:nicknames #:pregex)
    (:use #:cl))

;; ------------------------------
;; Debugging aids

#+allegro
(defvar *wrapped-functions* nil)

#+allegro
(defmacro-exported fast-wrap ((function &rest arglist) &rest expressions)
  (let ((wrapper-sym (gensym)))
  `(progn
     (excl:def-fwrapper ,wrapper-sym ,@(if arglist arglist '(&rest args))
       ,@expressions
       (excl:call-next-fwrapper))
     (excl:fwrap (symbol-function ',function) 'fast-wrap ',wrapper-sym)
     (pushnew ',function *wrapped-functions*))))

#+allegro
(defun-exported fast-unwrap-all ()
  (mapc #'(lambda (function)
	    (excl:funwrap function 'fast-wrap))
	*wrapped-functions-hash*))

#+allegro
(defmacro-exported fast-unwrap (function)
  `(excl:funwrap ',function 'fast-wrap))

;; -------------------------------
;; Things to do at the interactive prompt

(defmacro probe (x)
  "Describe and return the value of a symbol, if any."
  `(progn 
     (describe ',x)
     (if (documentation ',x)
	 (pprint (documentation ',x)))
     (if (boundp ',x) ,x nil)))

(defun ensure-scanner (expr &rest options)
  (cond ((symbolp expr)
	 (apply #'pregex:create-scanner (cons (symbol-name expr) options)))
	((stringp expr)
	 (apply #'pregex:create-scanner (cons expr options)))
	(t expr)))

(defun-exported grep (regex data)
  (let* ((scanner (ensure-scanner regex :case-insensitive-mode t :multi-line-mode t)))
    (on-trees (nconc left right)
	      (if (and (stringp it) (pregex:scan scanner it))
		  (mklist it)
		nil)
	      data)))

;; ORIGINAL:		  
;;    (labels ((grep-int (data)
;;	      (cond ((null data)
;;		     nil)
;;		    ((stringp data)
;;		     (if (pregex:scan regex-opt data)
;;			 (mklist data)))
;;		    ((consp data)
;;		     (nconc (grep-int (car data))
;;			    (grep-int (cdr data))))
;;		    (t nil))))
;;      (grep-int data))))



#|
(defun-exported find-matching-symbols (regex &key (packages think.build:*think-base-packages*))
  "Return the symbols matching the regex"
  (let ((scanner (create-scanner regex :case-insensitive-mode t :multi-line-mode t)))
    (labels ((match-package (package-name)
			    (awhen (find-package package-name)
				   (do-symbols (s it results)
				     (awhen (grep scanner (symbol-name s))
					    (push it results))))))
      (sort (remove-duplicates 
	     (mapcan #'match-package (mklist packages)))
	    #'equal))))


(defun-exported print-matching-symbols (regex packages)
  "Print a list of the symbols matching the regex"
  (aif (find-matching-symbols regex :packages package)
       (print-as-atoms it)
       "Not found"))

(defun-exported find-functions (regexes &key (packages think.build:*think-base-packages*) (printable nil) (internal nil))
  "Find all functions in packages with documentation strings
   matching the provided regular expression"
  (let ((scanners ;; precompute scanners for each member of conjunction
	 (mapcar #'(lambda (exp) 
		     (ensure-scanner exp :case-insensitive-mode t :multi-line-mode t))
		 (mklist regexes)))
	(results nil))
    (labels ((examine-symbol (sym) ;; if symbol matches, expand
	       (awhen (documentation sym 'function)
		 (when (every #'(lambda (scanner) (grep scanner it)) scanners)
		   (if printable
		       (push (list (package-name (symbol-package sym)) (symbol-name sym) it) results)
		     (push sym results)))))
	     (match-package (package-name) ;; walk through package symbols
		(setf results nil)
		(awhen (find-package package-name)
		   (if internal
		       (do-symbols (s it results) 
			 (examine-symbol s))
		     (do-external-symbols (s it results)
			(examine-symbol s))))))
      (mapcan #'match-package (mklist packages)))))

(defun-exported print-functions (regexes &key (packages think.build:*think-base-packages*) (internal nil))
  (let ((defs (find-functions regexes :packages packages :printable t :internal internal)))
    (dolist (def defs)
      (dbind (pkg sym doc) def
	(format t "~A:~A~%~A~%~%" pkg sym doc)))))
|#

(defun-exported print-as-atoms (data)
  "Print list and cons data sequentially to the repl."
  (on-trees (progn left right) (pprint it) data))

(defun-exported find-list-loop (list &optional (max 10000))
  (let ((priors (hash)))
    (labels ((rec (list prev pos)
		  (let ((elt (car list)))
		    (if (>= pos max)
			(format t "Maxed out at ~A" max)
		      (if (hash-get priors elt)
			  (format t "Found loop: ~A after ~A at ~A." elt prev pos)
			(progn
			  (hash-put priors elt t)
			  (if (null (cdr list))
			      (format t "End of list at ~A" pos)
			    (rec (cdr list) elt (1+ pos)))))))))
      (rec list nil 1))))


;;  (cond ((atom data)
;;	 (print data))
;;	((listp data)
;;	 (dolist (elt data)
;;	   (print-as-atoms elt)))
;;	((consp data)
;;	 (print-as-atoms (car data))
;;	 (print-as-atoms (cdr data)))))

#|
(defun-exported walk-macroexpand (expression &key all)
  "Should work for most macro setups unless an inner
   macro depends on an environment setup by a :cl 
   package macro which this doesn't expand.
   Note 1: need exception list for symbol-macrolet, 
   macrolet and other macro environment modifiers
   from cl.
   Note 2: need descent rules for build-in special
   forms so we don't expand non-expandable entities."
  (labels ((expandable-p (token)
	     (and (not (null token))
		  (not (consp token))
		  (macro-function token)
		  (or all (not (eq (symbol-package token) (find-package :cl))))))
	   (expand-next (curr-expr)
	     (cond ((null curr-expr)
		    nil)
		   ;; symbol macrolet
		   ((and (symbolp curr-expr) (expandable-p curr-expr))
		    (macroexpand-1 curr-expr))
		   ;; all other vars, constants, etc.
		   ((atom curr-expr)
		    curr-expr)
		   ;; car of list is macro
		   ((and (consp curr-expr) 
			 (expandable-p (car curr-expr)))
		    (macroexpand-1 curr-expr))
		   ;; car of the list is not on our stoplist
		   ((and (consp curr-expr)
			 (not (consp (car curr-expr)))
			 (symbolp (car curr-expr))
			 (not (equal (symbol-name (car curr-expr)) (symbol-name :quote))))
		    (mapcar #'expand-next curr-expr))
		   (t curr-expr))))
    (loop while t do
      (pprint expression)
      (let ((next-expr (expand-next expression)))
	(when (equalp next-expr expression)
	  (return t))
	(setq expression next-expr)
	(print "Continue? ('n' to exit)")
	(when (eq (read-char) #\n)
	  (return nil))))))
|#	   