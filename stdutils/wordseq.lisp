;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          wordseq.lisp
;;;; Purpose:       Word sequence implementation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

;; ----------------------------------------------------
;; Word Sequences 
;; 
;; Efficient representation for large collections
;; of strings with many common subsequences.
;;
;; NOTE: Might do better on storage & access time
;; with a really large character array or set of
;; character arrays.  Could store length at target
;; offset in two characters then subseq out the
;; remaining chars to return a word.
;; O(1) lookup, efficient storage of data
;;
;; Finding an existing word would require O(n) though
;; we could use a small index to declare the offset of
;; of words beginning in 3 char subsequences
;; ----------------------------------------------------

(in-package :stdutils)

(defvar *word-ids* (make-hash-table :size 1000 :rehash-size 1.5 :test #'equal))  ;; hash : String to Id
(defvar *ids-word* (make-hash-table)) ;; id to string
(defvar *word-count* 0)

#|
;; Import/export
(defun save-wordseq (fasl)
  (fasl-write *word-ids* fasl)
  (fasl-write *ids-word* fasl)
  (fasl-write *word-count* fasl))

(defun load-wordseq (data)
  (setf *word-ids* (first data))
  (setf *ids-word* (second data))
  (setf *word-count* (third data)))
|#

;; To re-init, not exported
(defun-exported init-wseq () 
  (setf *word-ids* (make-hash-table :size 1000 :test #'equal :rehash-size 1.5 :rehash-threshold 0.4))
  (setf *ids-word* (make-hash-table :size 1000 :test #'eq :rehash-size 1.5 :rehash-threshold 0.4))
  (setf *word-count* 0))

;; Exposed API

(defun-exported wseq-word (int)
  (gethash int *ids-word*))

(defun-exported wseq-wordlist (wseq)
  "Return the ordered list of words represented by this string.
   Do not side effect these strings."
  (map 'list #'(lambda (x) 
		 (wseq-word x))
       wseq))

(defun-exported wseq-name (wseq)
  "Return the concatenated string this wseq represents.  Duplicates words in memory"
  (string-left-trim " "
		    (apply #'concatenate
			   (cons 'string
				 (map 'list #'(lambda (x)
					    (concatenate 'string " " (wseq-word x)))
				      wseq)))))

(defun-exported wseq-subseq (subseq target)
  (labels ((match (offset)
	      (loop for i from 0 to (1- (length subseq))
		finally (return t) do
		(if (neq (aref subseq i)
			 (aref target (+ i offset)))
		    (return nil)))))
    (loop for word across target 
	 for i from 0 
	 finally (return nil) do
	   (if (eq word (aref subseq 0))
	       (if (match i)
		   (return i))))))


;; Apply 
(defmacro-exported mapwords (fn wseq)
  (let ((sym (gensym)))
    `(dowords (,sym ,wseq) (,(eval fn) ,sym))))
  

;; (dowords (var result) seq &rest body)
(defmacro-exported dowords ((var wseq &optional (result nil)) &rest body) 
  "(dowords (var result) seq &rest body) This macro iterates through the string representation of the words in wseq."
  `(let (,var)
     (do* ((i 0 (1+ i)))
	 ((= i (length ,wseq)) ,(when result result))
       (setf ,var (gethash (aref ,wseq i) *ids-word*))
       ,@body)))
	 
(declaim (inline =wseq))
(defun-exported =wseq (ws1 ws2)
  (declare 
   (type (simple-array fixnum (*)) ws1 ws2)
   (inline length aref)
   (optimize (speed 3) (safety 0)))
  (if (eq ws1 ws2)
      t
    (let ((len (length ws1)))
      (when (= len (length ws2))
	(dotimes (i len t)
	  (when (not (= (aref ws1 i) (aref ws2 i)))
	    (return nil)))))))

(defun-exported >wseq (ws1 ws2)
  (declare 
   (type (simple-array fixnum *) ws1 ws2)
   (optimize (speed 3) (safety 0)))
  (let ((len1 (length ws1))
	(len2 (length ws2)))
    (if (= len1 len2)
	(dotimes (i len1 nil)
	  (let ((a1 (aref ws1 i))
		(a2 (aref ws2 i)))
	    (cond ((> a1 a2) (return t))
		  ((< a1 a2) nil))))
      (> len1 len2))))


;; Character array version
;; faster, but ugly
(defun-exported make-wseq (str)
  (declare (type (simple-array character *) str)
	   (optimize (speed 3) (safety 0) (debug 0))
	   (inline subseq force-wseq-word char-downcase))
  (let ((len (length str))
	(start 0)
	(end 0)
	(words nil))
    (declare (type fixnum len start end)
	     (type list words))
    (do ((ch (char-downcase (char str end)) (char-downcase (char str end))))
	((eq (1+ end) len))
      (incf end)
      (when (eq ch #\  )
	(setf words (cons (force-wseq-word (subseq str start (1- end))) words))
	(setf start end)))
    (make-array
     (1+ (length words))
     :element-type 'fixnum
     :initial-contents (nreverse (cons (force-wseq-word (subseq str start)) words)))))

;; Extract words, make word vals & turn into vector
;; <cleaner version for reference>
(defun make-wseq-old (str)
  (let (words)
    (do* ((start 0 (1+ end))
	  (end -1 (position (character " ") str :start start))
	  (word nil (if (null end) 
		      (subseq str start)
		      (subseq str start end))))
	((null end) (make-array
		     (1+ (length words))
		     :element-type 'fixnum
		     :initial-contents (nreverse (cons (force-wseq-word word) words))))
      (when word (setf words (cons (force-wseq-word word) words))))))


;; ========================
;; Internal functions

(defun-exported force-wseq-word (word)
  (let ((id (gethash word *word-ids*)))
    (if id
      id
      (let ((id (unique-word-id)))
	(setf (gethash word *word-ids*) id)
	(setf (gethash id *ids-word*) word)
	id))))

(defun unique-word-id ()
  (incf *word-count*))

    
	

