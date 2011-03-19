;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          arrays.lisp
;;;; Purpose:       Random array utilities
;;;; Programmer:    Ian S Eslick
;;;; Date Started:  May 2005
;;;;
;;;; *************************************************************************

(in-package :stdutils)

(defun-exported fast-array-copy (a1 a2 start count)
  "Unsafe array copy"
  (declare #-(or mcl sbcl) (type (vector fixnum) a1 a2)
	   #-(or mcl sbcl) (type fixnum start count)
	   (optimize speed (safety 0)))
  (loop for pos fixnum from start to (- count 1) do
       (setf (svref a2 pos) (svref a1 pos))))

(defun-exported vector-1d-lshift (array index amount &key (adjust t))
  (declare (type array array)
	   (type fixnum index amount)
	   (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (loop for i from index upto (- (length array) amount 1) do
       (setf (aref array i) (aref array (+ i amount))))
  (if (array-has-fill-pointer-p array)
      (setf (fill-pointer array) (- (fill-pointer array) amount))
      (when adjust
	(setf array (adjust-array array (- (length array) amount)))))
  array)

(defun-exported vector-1d-rshift (array index amount &key filler (adjust t))
  (declare (type array array)
	   (type fixnum index amount)
	   (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (when adjust 
    (setf array (adjust-array array (+ (length array) amount))))
  (when (and 
	 (array-has-fill-pointer-p array)
	 (adjustable-array-p array)
	 (> (+ (fill-pointer array) adjust) (length array)))
    (setf array (adjust-array array (+ (length array) amount))))
  (loop for i from (- (length array) 1 amount) downto index do
       (setf (aref array (+ i amount)) (aref array i)))
  (when filler
    (loop for i from index upto (1- (+ index amount)) do
	 (setf (aref array i) filler)))
  array)

