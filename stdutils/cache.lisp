;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cache.lisp
;;;; Purpose:       Various cache implementations with generic func interface.
;;;;                Uses the element as structure model.
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004

(in-package :stdutils)

;;;; ------------------------------------
;;;; CACHE OBJECTS
;;;; ------------------------------------

;;;; A simple hash & queue object cache with least-recently-used 
;;;; replacement policy

(defclass-exported object-cache-h-q () 
  ((index :type hash-table :accessor object-cache-h-q-index)
   (queue :type list-queue :accessor object-cache-h-q-queue)
   (max-size :type fixnum :accessor object-cache-h-q-max-size)
   (size :initform 0 :type fixnum :accessor object-cache-h-q-size))
  (:documentation "A cache of objects using arbitrary keys"))

(defmethod-exported initialize ((c object-cache-h-q) &rest rest)
  (with-keywords (test size) rest
    (with-slots (index queue max-size) c
      (setf index (hash :test (aif test it 'eql)))
      (setf queue (make-instance 'list-queue))
      (setf max-size size)))
  c)

;; Interface

(defmethod-exported cache-objects ((c object-cache-h-q))
  (object-cache-h-q-size c))

(defmethod-exported cache-max-objects ((c object-cache-h-q))
  (object-cache-h-q-max-size c))

;; Index contains map of key to pairs, ( queue-elt, object )
;; Queue contains index keys
(defmethod-exported write-cache ((c object-cache-h-q) key object &key force)
  "Ensure the object is in the cache; return the object"
  (declare (ignore force))
  (with-slots (index queue size max-size) c
    (assert (= size (hash-table-count index)))
    (assert (= size (queue-size queue)))
    ;; Track qelt for this key for fast remove/promote
    (let ((qelt 
	   (aif (hash-get index key)
		(progn
		  ;; if in cache; reorder on write
		  (queue-element-to-front queue (car it))
		  (car it))
		(progn
		  ;; If not in cache
		  (enqueue queue key)
		  (incf size)
		  ;; return new queue element
		  (queue-head queue)))))
      ;; index of objects and queue elts
      (hash-put index key (cons qelt object))
      (when (>= size max-size)
	(drop-entry c nil))
    object)))

(defmethod-exported read-cache ((c object-cache-h-q) key)
  (with-slots (index queue size) c
    (let ((record (hash-get index key)))
      (unless record (return-from read-cache nil))
      (dbind (qelt . obj) record
	     (queue-element-to-front queue qelt)
	     obj))))

(defmethod-exported drop-entry ((c object-cache-h-q) key)
  "Drops the oldest entry if nil.  If key is valid but not
   present than no side effects occur"
  (with-slots (index queue size) c
    (if (= 0 size) nil
      (if key
	  (let ((record (hash-get index key)))
	    (when record
	      (dbind (qelt . obj) record
		      (declare (ignorable obj))
		      ;; grody, but need to break queue abstraction for this
		      (list-remove queue qelt)
		      ;; Remove from hash
		      (hash-rem index key)
		      ;; Decrement our count
		      (decf size)
		      (assert (= size (hash-table-count index)))
		      (assert (= size (queue-size queue)))
		      t)))
	(let ((key (peek queue :location :bottom)))
	  (if key 
	      (drop-entry c key)
	    nil))))))

(defmethod-exported flush-cache	((c object-cache-h-q))
  (with-slots (index queue size) c
      (loop until (or (null (drop-entry c nil))
		      (= size 0)))))


;; Size management
(defmethod-exported resize ((c object-cache-h-q) new-size)
  (with-slots (index queue size) c
    (let ((diff (- new-size size)))
      (when (< diff 0)
	(loop until (< size new-size) do
	  (drop-entry c nil)))
      diff)))


;;;; A top level macro for creating object caches

(defmacro-exported create-object-cache (&rest rest &key 
					(type 'object-cache-h-q)
					(size 1000)
					(test #'eql)
					&allow-other-keys)
  (let ((inst (gensym)))
    `(let ((,inst (make-instance ',type)))
       (initialize ,inst ,@(append (rem-keywords rest (list :test :type)) (list :test test :size size)))
       ,inst)))

;; --------------- tests

;;(defun write-cache-n (c n)
;;  (map0-n #'(lambda (n) (write-cache c n n)) n))

(defmethod-exported map-cache ((c object-cache-h-q) fn &key oldest-first)
  (with-slots (index queue) c
    (map-queue queue
	       #'(lambda (key) (funcall fn (cdr (hash-get index key))))
	       :reverse oldest-first)))
	       

(defmethod-exported show-cache ((c object-cache-h-q) &key oldest-first)
  (map-cache c
	     #'(lambda (obj) (pprint obj) :reverse oldest-first))
  t)
