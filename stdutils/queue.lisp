;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queue.lisp
;;;; Purpose:       Various queue implementations with generic func interface.
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004

(in-package :stdutils)

;;--------------------------------
;; List Queue Class
;;--------------------------------

;;(defprotocol queue
;;    (enqueue dequeue 
;;     size empty-p clear
;;     get-iterator))
	     
(defclass-exported list-queue ()
  ((size :accessor queue-size :initform 0)
   (head :accessor queue-head :initform nil))
  (:documentation
   "Queue object, contains the head element and some stats"))

(defclass-exported list-queue-elt ()
  ((pred :initarg :pred :accessor pred)
   (succ :initarg :succ :accessor succ)
   (data :initarg :data :accessor get-data))
  (:documentation
   "An element forming a queue, the provided element
    is treated as a head for push/pop operations"))

(defmethod-exported queue-empty-p ((queue list-queue))
  (eq (queue-size queue) 0))

(defmethod-exported queue-clear ((queue list-queue))
  (setf (queue-size queue) 0
	(queue-head queue) nil))

(defmethod-exported list-queue-list ((queue list-queue))
  (let ((list nil))
    (map-queue queue (lambda (data) (push data list)))
    list))

(eval-when (compile eval load)
  (export (list 'queue-size 'queue-empty-p 'queue-head)))


;; List interface
(defmethod-exported list-remove ((q list-queue) (qe list-queue-elt))
  "Remove qe from its list"
  (case (queue-size q)
    (1 (progn
	 (queue-clear q)
	 qe))
    (0 (error "Cannot call list-remove when list is of size 0"))
    (t (with-slots (pred succ) qe
	 (setf (succ pred) succ)
	 (setf (pred succ) pred)
	 (decf (queue-size q))
	 qe))))

(defmethod-exported list-insert ((q list-queue) (qe (eql nil)) (new list-queue-elt))
  (setf (queue-head q) new)
  (with-slots (pred succ) new
    (setf pred new
	  succ new)
    (incf (queue-size q)))
  new)

(defmethod-exported list-insert ((q list-queue) (qe list-queue-elt) (new list-queue-elt))
  "Insert new in front of qe in the list"
  (with-slots (pred succ) qe
    (setf (succ new) qe
	  (pred new) pred
          (succ pred) new
	  pred new)
    (incf (queue-size q)))
  new)

(defmethod-exported search-for-entry ((q list-queue) item &key key (test #'eq) &allow-other-keys)
  (awhen (queue-head q)
    (search-for-entry it item :key key :test test :head it)))

(defmethod-exported search-for-entry ((q list-queue-elt) item &key key (test #'eq) head)
  (if (funcall test item
	       (if key (funcall key (get-data q)) (get-data q)))
      q
      (let ((next (succ q)))
	(when (and next (not (eq next head)))
	  (search-for-entry (succ q) item :key key :test test :head head)))))

;; Queue Interface

(defmethod-exported peek ((q list-queue) &key (location :top))
  (with-slots (head) q
    (when head
      (ecase location
	(:top (get-data head))
	(:bottom (get-data (pred head)))))))

(defmethod-exported push-front ((q list-queue) datum)
  (if (= (queue-size q) 0) (enqueue q datum)
      (list-insert q (pred (queue-head q))
		   (make-instance 'list-queue-elt
				  :pred nil
				  :succ nil
				  :data datum))))

(defmethod-exported enqueue ((q list-queue) datum)
  (with-slots (head size) q
    (let ((new (make-instance 'list-queue-elt
			      :pred nil
			      :succ nil
			      :data datum)))
	(when (= size 0)
	  (with-slots (pred succ) new
	    (setf pred new)
	    (setf succ new)))
	(setf head (list-insert q head new)))
    (get-data head)))
  
(defmethod-exported dequeue ((q list-queue))
  (with-slots (head size) q
    (unless (<= size 0)
      (let ((record (list-remove q (pred head))))
	(assert (>= size 0))
	(when record
	  (get-data record))))))

(defmethod-exported predecessor ((q list-queue) &optional item)
  (with-slots (head) q
      (if item
	  (get-data (pred (search-for-entry head item)))
	(get-data (pred head)))))

(defmethod-exported successor ((q list-queue) &optional item)
  (with-slots (head) q
      (if item
	  (get-data (succ (search-for-entry head item)))
	(get-data (succ head)))))

;; Miscellaneous useful utilities

(defmethod-exported queue-element-to-front ((queue list-queue) element)
  (list-remove queue element)
  (list-insert queue (queue-head queue) element)
  (setf (queue-head queue) element))

(defmethod-exported map-queue ((q list-queue) fn &key reverse)
  "Traverse the queue from most to least recent push collecting
   the results of applying fn to the data in each queue position"
  (with-slots (head size) q
    (if (null head)
	nil
      (let ((next (if reverse #'pred #'succ))
	    (start (if reverse (pred head) head)))
	(loop for n from 1 to size
	  collecting (funcall fn (get-data start)) into list 
	  finally (return list) do
	  (setf start (funcall next start)))))))

;;
;; An MP-safe version of this data structure
;;

(defclass-exported safe-list-queue (safe-queue-mixin safe-list-mixin list-queue) ())
