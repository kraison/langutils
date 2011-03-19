;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sbtree.lisp
;;;; Purpose:       A B+ tree definition
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

;; ====================================
;; A B+-tree for string tagged objects
;;
;; create: O(n)
;; find:   O(log n)
;; delete: O(log n)
;; insert: O(log n)
;;
;; This can be used as a set of strings with cheap membership
;; testing or as a storage for objects tagged by a string label
;; using the :key keyword parameter to extract the string from
;; the object on which to search.  Each tree is unique so once
;; you init the tree with a key search, it always uses that keyword
;; function
;; ====================================

(in-package :stdutils)


(defstruct (sbtree-node (:conc-name sbtree-))
  (char :type 'character)
  (left nil)
  (right nil))
  

;; Returns a new sbtree on which the succeeding operations can operate
;; - Assumes an ordered list of strings as input
(defun make-sbtree (objlist &key (key #'identity))
  (cons key (make-sbtree-int objlist key (length objlist))))

(defun make-sbtree-int (objlist key len)
  (let* ((start objlist)
	 (lmid (nth (1- (/ len 2)) objlist))
	 (rmid (cdr rmid)))
;;    (make-sbtree-node 
;;     :char (char 0 (car rmid))
;;     :left (make-sbtree-int start)
    ))    

;; Insert a new element into the tree
(defun sbtree-insert (elt tree))

;; Delete an element from the tree
(defun sbtree-delete (elt tree))

;; Get an element from the tree
(defun sbtree-get (elt tree))

;; Predicate to test whether the element exists in tree
(defun sbtree-memberp (elt tree))
  