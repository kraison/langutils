;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          regex-utils
;;;; Purpose:       Simple regex utilities good for language processing
;;;;                
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004
;;;;

(in-package :stdutils)

(defun-exported merge-or-regex-strings (list &optional (operation #\|))
  "Combine multiple strings by concatenting a separator, such
   as an OR for or'ing together regular expressions."
  (string-right-trim-one-char operation
     (concat-separated-strings operation list)))

;;
;; Ways of creating filter functions around regular expressions from strings
;;

(defmacro-exported make-regex-verify-filter (regex &optional (comment ""))
  (let ((scanner (cl-ppcre:create-scanner regex))
	(text (gensym)))
    `(lambda (,text &key print)
       (when print (pprint ,comment))
       (if (cl-ppcre:scan ,scanner ,text)
	   ,text
	 ""))))

(defmacro-exported make-regex-replace-filter (regex subst &optional (comment ""))
  `(let ((scanner (cl-ppcre:create-scanner ,regex)))
     (labels ((scanner-promise ()  ;; hack to get around fasl save problem
      	      (ensure-a-value scanner (cl-ppcre:create-scanner ,regex))
	      scanner))
       (lambda (text &key print)
	 (when print (pprint ,comment))
	 (cl-ppcre:regex-replace-all (scanner-promise) text ,subst)))))

(defun-exported filter-text (text filters &key (debug nil))
  "Successively applies a filter function from list to text
   and the next filter to the result of the prior"
  (accumulate-init #'(lambda (val filter)
		       (when debug 
			 (pprint val)
			 (y-or-n-p "Continue? "))
		       (apply filter (cons val (if debug '(:print t) '()))))
		   text
		   filters))
