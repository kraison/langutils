;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          collections.lisp
;;;; Purpose:       Various forms of collection classes and objects
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;


;; ====================================
;; Statistical Utilities and Histograms
;; ====================================

;; This file contains various kinds of collections
;; that are useful for tracking and indexing groups
;; of objects.  Each collection has different time/space
;; tradeoffs and are efficient at different scales.

;; CLOS inheritence and interface generation macros are 
;; both used to capture various modes of utility.

(in-package :stdutils)


;; ---------------------------------
;; Tag lists 
;; 
;; Little data abstraction for integer-tagging
;; larger structures so we can keep track of them
;; cheaply and avoid replication.  Intended for
;; short singleton tag records. 
;;
;; O(n) cost in space/time

(defun-exported make-taglist (&optional (test #'eql))
  "Simple data structure 'taglist'.  This function creates an 
   object that can provide a unique tag for each unique object 
   as determined under test.  Conceptually a uniqueified alist
   intended for small sets of objects"
  (cons nil test))

(defmacro-exported deftaglist (name &key (test #'eql))
  "A def macro form for the defun make-taglist"
  `(setq ,name (make-taglist ,test)))

;; 200000 lookups takes 2.235 seconds

(defun-exported taglist-tag (object taglist)
  "Take an arbitrary object and a 'taglist' data structure and 
   return a unique integer id that can be used to retrieve that
   object."
  (dbind (list . test) taglist
    (let ((tag (position object list :test test)))
      (unless tag
	(progn (setf (car taglist) (append list (list object)))
	       (setf tag (1- (length (car taglist))))))
      tag)))

(defun-exported taglist-object (tag taglist)
  "Given a tag and a taglist return the object designated
   by the tag"
  (nth tag (car taglist)))
    
;; Returns a vector of tag values for the given words
(defun-exported tags-from-words (words taglist &key (tagf #'taglist-tag))
  (let ((seq (make-array (length words))))
    (map-into seq 
	      #'(lambda (word) 
		  (funcall tagf word taglist))
	      words)))
  
;; Returns a list of words for the given tags
(defun-exported words-from-tags (tags taglist &key (tagf #'taglist-object))
  (map 'list #'(lambda (tag) 
		 (funcall tagf tag taglist))
       tags))
    

;; ---------------------------------
;; Numeric ID generator
;;
;; A simple numeric id generator class, generates
;; unique id's within the id space of the class and
;; contains a predicate for testing range validity.
;;
(defclass+-exported id-generator ()
  ((id nil))
  (:prefix #:id-generator- ))

(defmethod-exported new-id ((idgen id-generator))
  (incf (id-generator-id idgen)))

(defmethod-exported valid-id-p ((idgen id-generator) id)
  (and (> id 0) (<= id (id-generator-id idgen))))



;; ---------------------------------
;; Bi-Directional Named Index
;;
;; Class wrapper around the hashtable primitive
;; that allows us to store (id object) pairs
;; in a bi-directional O(1) time index.
;; The object lookups are based on #'eq for 
;; efficiency so strings are not valid id's.
;; See vechash.cl and the above tag-list for
;; ways to efficiently index on multi-word
;; string names.

(defclass+-exported bidir-index ()
  ((by-id (hash))
   (by-obj (hash)))
  (:prefix #:bidir-index- ))

(defmethod-exported index-get-by-id ((bidx bidir-index) id)
  (let ((index (bidir-index-by-id bidx)))
    (hash-get index id)))

;; Do not edit the hash returned by this method (NOTE: make copy later?)
(defmethod-exported index-get-id-obj-map ((bidx bidir-index))
  (bidir-index-by-id bidx))
    
(defmethod-exported index-get-by-obj ((bidx bidir-index) obj)
  (let ((index (bidir-index-by-obj bidx)))
    (hash-get index obj)))

(defmethod-exported index-put ((bidx bidir-index) id obj)
  (let ((id-index (bidir-index-by-id bidx))
	(obj-index (bidir-index-by-obj bidx)))
    (hash-put id-index id obj)
    (hash-put obj-index obj id)))

(defmethod-exported index-drop ((bidx bidir-index) id)
  (with-slots (by-id by-obj) bidx
      (let ((obj (hash-get by-id id)))
	(hash-rem by-id id)
	(hash-rem by-obj obj))))

;; ----------------------------------
;; Object Registry
;;
;; Hand an object to the registry to track it
;; the registry provides bi-directional mappings
;; and id generation.  The create method returns
;; the id handle of the object to be used to pull
;; it out again.  For moderate-to-large collections.
;; The registry can be named and given an id itself.
;; [Use tag-list for small collections (assoc list)]

(defclass+-exported object-registry (bidir-index id-generator)
  ((name nil)
   (id 0))
  (:prefix #:object-registry- ))

(defmethod-exported register ((registry object-registry) object)
  (let ((id (new-id registry)))
    (index-put registry id object)
    id))

(defmethod-exported drop ((registry object-registry) id)
  (index-drop registry id))

;; Shadow id creation so you can't mess up the 
;; without registring an object
;;(defmethod-exported new-id ((registry object-registry))
;;  (call-next-method registry))



;; -----------------------------------
;; Scalable Copying Set
;;
;; Efficient sets that scale from small to
;; large allowing all sorts of set ops.
;; copying means operations like 'union' 
;; creates new sets)

(defclass+-exported scalable-set ()
  ((type :list)
   (rep  nil)
   (membership #'eq))
  (:prefix #:set- ))

;; Generate an exported method that dispatches to the representation
;; based on the type tag in the set object for the class scalable-set
;; Captures 'rep', 'type', 'test' and 'set'
(defmacro-exported create-set-method-inst (op-type name args list-form hash-form)
  `(defmethod-exported ,name ((set scalable-set) ,@args)
     (let* ((rep (set-rep set))
	    (type (set-type set))
	    (test (set-membership set))
	    (inst ,(if (eq op-type :new)
		       `(make-instance 'scalable-set :type type :membership test)
		     `nil)))
       (cond ((eq type :list)
	      ;; Ensure that mutating ops update the set-rep place properly
	      ,(cond ((eq op-type :mutate-place)
		     `(progn 
			(let ((new (setf (set-rep set) ,list-form)))
			  (set-expand set)
			  new)))
		     ;; Ensure that ops return new structures within class!
		     ((eq op-type :new)
		      `(progn 
			 (setf (set-rep inst) ,list-form)
			 inst))
		     (t `,list-form)))
	     ((eq type :hash)
	      ;; Ensure that ops return new structures within class!
	      ,(if (eq op-type :new)
		   `(progn 
		      (setf (set-rep inst) ,list-form)
		      inst)
		 `,hash-form))
	     (t (error "invalid type in class 'scalable-set'" type rep))))))

(defparameter *scalable-set-threshold* 20)
(defmethod set-expand ((set scalable-set))
  (let ((rep (set-rep set)))
    (when (> (length rep) *scalable-set-threshold*)
      (setf (set-type set) :hash)
      (setf (set-rep set) (hash :keyvals (cons rep rep))))))

;; Interface definition
(defparameter *set-interface-definition*
    '((:mutate-place set-put (item) 
	       (pushnew item rep :test test)
	       (hash-put rep item item))
      (:predicate set-member (item)
       (member item rep :test test)
       (hash-get rep item))
      (:mutate-place set-rem (item)
       (remove item rep :test test)
       (hash-rem rep item))
      (:new set-union (set2)
       (union rep (set-rep set2))
       (hashset-union rep (set-rep set2)))
      (:new set-intersection (set2)
       (intersection rep (set-rep set2))
       (hashset-intersection rep (set-rep set2)))
      (:new set-diff (set2)
       (set-difference rep (set-rep set2))
       (hashset-difference rep (set-rep set2)))))

;; Simple loop over the interface definition record
(defun-exported create-set-method-if ()
  (loop for record in *set-interface-definition* do
	`(create-set-method-inst ,@record)))

;; Create the methods
(create-set-method-if)


