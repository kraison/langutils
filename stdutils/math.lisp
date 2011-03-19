;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          math.lisp
;;;; Purpose:       Math related utility functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004

(in-package :stdutils)

;; -------------------------
;; Simple math utilities

(eval-when (compile eval load)
  (proclaim '(optimize speed)))

(defun-exported limit-max (limit value)
  (if (> value limit) limit value))

(defun-exported limit-min (limit value)
  (if (< value limit) limit value))

;; ------------------------
;; Simple

(defun-exported factorial (N)
  (labels ((fact-rec (N P)
	     (if (<= N 1)
		 P
		 (fact-rec (1- N) (* P N)))))
    (fact-rec N 1)))

;; ------------------------
;; Counting

(defun-exported integer-permutations (N M &aux (p 1))
  "(n P m) from classic combinatorics.  Number of unique
   unordered sets"
  (cond ((or (null M) (= N M))
	 (setf p (factorial N)))
	((> N M)
	 (loop for i from N downto (1+ (- N M)) do
	      (setf p (* p i)))))
  p)

(defun-exported integer-combinations (N M)
  "(n C m) from classic combinatorics.  Number of unique
   ordered sets"
  (/ (integer-permutations N M) (factorial M)))

(defun-exported integer-permutations-repeated (N M repeats)
  "Permutations with repeated elements.  Repeats is the number of
   subsets of N that are counted as identical under permutation"
  (/ (integer-permutations N M) (apply #'* (mapcar #'factorial repeats))))