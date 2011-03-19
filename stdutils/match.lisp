;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          match.lisp
;;;; Purpose:       General symbolic pattern matching utilities
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;
;;;; This file should be copyright Paul Graham because it's copied from his OnLisp book

;;;; This code is copyright 1993 by Paul Graham, but anyone who wants 
;;;; to use the code in any nonprofit activity, or distribute free
;;;; verbatim copies (including this notice), is encouraged to do so.

(in-package :stdutils)

(defun-exported match (x y &optional binds)
  (acond-mv ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
	    ((binding x binds) (match it y binds))
	    ((binding y binds) (match x it binds))
	    ((varsym? x) (values (cons (cons x y) binds) t))
	    ((varsym? y) (values (cons (cons y x) binds) t))
	    ((and (consp x) (consp y) (match (car x) (car y) binds))
	     (match (cdr x) (cdr y) it))
	    (t (values nil nil))))

(defun varsym? (x)
  (if (symbolp x) 
      (let ((name (symbol-name x)))
	(unless (equal name "")
	  (eq (char (symbol-name x) 0) #\?)))))

(defun binding (x binds)
  (labels ((recbind (x binds)
		    (aif (assoc x binds)
			 (or (recbind (cdr it) binds)
			     it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defmacro-exported case-match-bind (data &rest cases)
  "Produces an expression that when evaluated takes the
   value of 'data' and tries to perform a match using
   the static patterns in the car of each case in cases.
   If the pattern matches, it's variable names are bound
   to the matched values in the environment of the body
   expression of that case. 
    
   (setf var '(1 2 3))
   (case-match-bind var ((?A) ?A) ((?A ?B) ?B)) ==> 2

   (setf pat1 '(?A))
   (case-match-bind var (pat1 ?A) ((?A ?B) ?B)) ==> error"
  (let ((value (gensym)))
    (flet ((gen-clause (clause)
	     (destructuring-bind (pat do) clause
	       (if (eq pat t)
		   clause
		 `((match ',pat ,value)
		   (alist-bind ,(vars-in-pat pat) it
			       ,do))))))
      (let ((clause-list (mapcar #'gen-clause cases)))
	`(let ((,value ,data))
	   (acond ,@clause-list))))))

;; Only works for textual patterns
;; (case-match-bind exp
;;    ((~ (| ?A ?B))
;;     `(& (~ ?A) (~ ?B)))
;;    ...)


	   
;; NOTE: Need to finish
(defmacro-exported if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in-pat pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defun-exported vars-in-pat (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (varsym? expr) (list expr))
    (union (vars-in-pat (car expr) atom?)
	   (vars-in-pat (cdr expr) atom?))))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq) 
                             (destruc pat gseq #'simple?))
                       then 
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
    (cond ((gensym? pat)
           `(let ((,pat ,expr))
              (if (and (typep ,pat 'sequence)
                       ,(length-test pat rest))
                  ,then
                  ,else)))
          ((eq pat '_) then)
          ((varsym? pat)
           (let ((ge (gensym)))
             `(let ((,ge ,expr))
                (if (or (gensym? ,pat) (equal ,pat ,ge))
                    (let ((,pat ,ge)) ,then)
                    ,else))))
          (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s) 
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))


;; ======================
;; Matching vectors
;; ======================

(defun match-arrays (offset pattern &key (elt 'aref) (cmp 'eq) (atom-type :symbol))
  (labels ((revert-unless (expr) ;; If testable expression fails, revert to old end
			  `(progn (mark) 
				  (if ,expr
				      t
				    (progn (revert) nil))))
	   (make-comparison (atom) `(if (= end end-max) nil ;; compare and increment if not over end-max
;;				      (progn (format t "comparing: ~A to data ~A at ~A~%" ',atom
;;						     (,elt (car vector) end) end)
				      (when (,cmp ,(case atom-type 
						     (:symbol `',atom)
						     (:integer `,atom))
						  (,elt vector end))
					(incf end) t)))
	   (make-and (list) (revert-unless `(and ,@(expand-pattern list))))
	   (make-or (list) (revert-unless `(or ,@(expand-pattern list))))
	   (make-? (list) `(progn ,(expand-pattern list) t))
	   (make-* (list) `(progn (loop while ,(expand-pattern list) do (progn nil)) t))
	   (make-+ (list) `(when ,(expand-pattern list) ,(make-* list) t))
	   (make-last-1 (list) `(unless (= end 0)
				  (mark)
				  (setf end (1- end))
				  (if ,(expand-pattern list) t
				    (progn (revert) nil))))
	   (expand-pattern (list)
;;			(write-log vector-match "expand: ~A -- ~A~%" list (if (consp list) (car list) list))
			(cond ((null list) nil)
			      ((consp (car list)) 
			       (cons (expand-pattern (car list))
				     (expand-pattern (cdr list))))
			      (t (case (localize-symbol (car list) :package (find-package 'keyword))
				   (:* (make-* (cdr list)))
				   (:+ (make-+ (cdr list)))
				   (:? (make-? (cdr list)))
				   (:AND (make-and (cdr list)))
				   (:OR (make-or (cdr list)))
				   (:last-1 (make-last-1 (cdr list)))
				   (t (cons (make-comparison (car list))
					    (expand-pattern (cdr list)))))))))
      `(let ((end ,offset)
	     (mark ,offset))
	 (labels ((mark () (setq mark end))
		  (revert () (setq end mark)))
	   (declare (inline mark revert)
		    (type fixnum end mark))
	   (when ,(expand-pattern pattern)
	     (cons ,offset (max (1- end) ,offset)))))))
	   
(defun-exported find-all-patterns (pattern vector)
  (let ((pfunc (if (consp pattern) (compile-pattern pattern) pattern))
	(matches nil))
    (loop for offset from 0 to (1- (length vector)) do
      (aif (funcall pfunc vector offset)
	   (push it matches)))
    (nreverse matches)))

(defun-exported compile-pattern (pattern &key (elt 'aref) (cmp '=) (atom-type :integer))
  (compile nil (generate-code-search pattern :elt elt :cmp cmp :atom-type atom-type)))
	   
(defun-exported generate-code-search (pattern &key elt cmp atom-type)
  `(lambda (vector &optional (offset 0))
     (declare (optimize speed (safety 0))
	      (type array vector)
	      (type fixnum offset)
	      (inline aref eq = svref))
     (let ((end-max (length vector)))
       (declare (type fixnum end-max))
       ,(match-arrays 'offset pattern :elt elt :cmp cmp :atom-type atom-type))))

(defmacro-exported vector-match1 (pattern vector &optional (start-index 0))
  "Match pattern over vector starting at start-index."
  `(let ((vector ,vector)
	 (end-max (length ,vector)))
     (declare (type fixnum end-max)
	      (type (array fixnum) vector)
	      (optimize (speed 3) (safety 0) (debug 0)))
     ,(match-arrays start-index pattern)))

(defmacro-exported do-collect-vector-matches ((start-label end-label pattern)
					      (&rest vectors) &rest body)
  "The pattern must be state, ie a quoted list or a previously-defined variable"
  `(let* ((vector ,(car vectors))
	  (end-max (length ,(car vectors)))
	  (results nil))
     (when (or (< end-max 0) (> end-max 100000))
       (error "Range is too big in collect vector matches"))
     (loop for i from 0 upto (1- end-max) do
       (awhen ,(match-arrays 'i pattern)
	      (let ((,start-label (car it))
		    (,end-label (cdr it)))
		(setq i ,end-label)
		(awhen (progn ,@body)
		       (push it results)))))
     (nreverse results)))
