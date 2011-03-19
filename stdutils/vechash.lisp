;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vechash.lisp
;;;; Purpose:       Fast vector element hash implementation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils.gds)

;; ----------------------------------------------------------
;; VECTOR-HASH -- Vector Keyed Hash Trees
;;
;; Tree of hashes for fast lookup of integer simple-vectors keys
;; over very large vector spaces
;;
;; Assumptions:
;; * Vector elements are positive integers 1...n
;; * You are loading thousands of elements (expensive otherwise)
;; * Sparsely populated tree 
;; ----------------------------------------------------------

(defparameter *vector-keyed-threshold* 5
  "When to switch internally from assoc list to hash table")

(defclass vector-keyed-table (table)
  ((root :accessor table-root 
	 :initarg :root)
   (threshold :reader vhash-threshold
	      :initarg :threshold
	      :initform *vechash-threshold*)
   (count :accessor table-count
	  :initform 0)))

(defmethod initialize-instance :after ((table vector-keyed-table) &rest initargs &key &allow-other-keys)
  (clear table))

;;
;; ----------------------
;;

(defparameter *count-updated* nil)
	   
(defun make-vknode (&optional value table)
  (cons value table))

(defun vknode-value (node) 
  (car node))

(defsetf vknode-value rplaca)

(defun vknode-table (node) 
  (cdr node))

(defsetf vknode-table rplacd)

(defun end-of-vkey (vkey index) 
  (= (length vkey) (1+ index)))

(defun extend-vktable (table key)
  "Add a vk node to the table for key"
  (add table key (make-vknode)))

(defun upgrade-vknode (node)
  "Upgrade vktable entry from assoc to hashed when the size
   exceeds the vk threshold.  Do this to the table in the
   provided vknode. Also downgrade (is this a good idea as
   it may cause thrashing?)"
  (when node
    (let ((ctable (vknode-table node)))
      (when ctable
	(cond ((and (subtypep (type-of ctable) 'assoc-table)
		    (>= (size-of ctable) *vector-keyed-threshold*))
	       (setf (vknode-table node) (convert ctable 'hashed-table))))))))
;;	      ((and (subtypep (type-of ctable) 'hashed-table)
;;		    (< (size-of ctable) *vector-keyed-threshold*))
;;	       (setf (vknode-table node) (convert ctable 'assoc-table))))))))

(defun ensure-vktable (node)
  "Ensure that the provided node has a proper table for 
   the next recusion of vector-keyed-put"
  (aif-ret (vknode-table node)
	   (setf (vknode-table node) (make-instance 'assoc-table))
	   (vknode-table node)))

(defun vktable-get-extend (vktable node key &aux (table (ensure-vktable node)))
  "Get node from table.  If new node, update vktable item count
   add a new node to the table for key.  If table has exceeded
   size, upgrade it to a hashed table and return the new node"
  (aif-ret (get-value table key)
	   (unless *count-updated* 
	     (incf (table-count vktable))
	     (setq *count-updated* t))
	   (extend-vktable table key)
	   (upgrade-vknode node)
	   (get-value (vknode-table node) key)))

(defun vector-keyed-put (vktable vkey value)
  "Internal recursion to walk tree and add or modify a value for
   vector key: vkey"
  (setf *count-updated* nil)
  (labels ((rec (node index)
	     (let* ((key (aref vkey index))
		    (nextn (vktable-get-extend vktable node key)))
	       (if (end-of-vkey vkey index)
		   (progn (setf (vknode-value nextn) value) value)
		   (rec nextn (1+ index))))))
    (rec (make-vknode nil (table-root vktable)) 0)))

(defun drop-vknode-value (tnode stack)
  "Clear value in target node (tnode) and if 
   alist is size zero or nil, delete entry in 
   parent table (snode) and, if zero, also delete"
   (if (or (null (vknode-table tnode))
	   (empty (vknode-table tnode)))
       (dbind (key . nextn) (car stack)
	      (drop-vktable-entry key nextn (cdr stack)))
       (setf (vknode-value tnode) nil)))
   
(defun drop-vktable-entry (key node stack)
  (drop (vknode-table node) key)
  (when (and stack 
	     (empty (vknode-table node))
	     (null (vknode-value node)))
    (dbind (key . nextn) (car stack)
	   (drop-vktable-entry key nextn (cdr stack)))))

(defun vector-keyed-rem (vktable vkey)
  "Remove a vector keyed value from the vktable
   and clean up any empty nodes or tables created
   thereby.  Also decrement the count"
  (let ((stack nil))
    (labels ((rec (node index)
	       (when node
		 (let* ((key (aref vkey index))
			(table (vknode-table node))
			(nextn (when table (get-value table key))))
		   (push (cons key node) stack)
		   (when nextn
		     (if (end-of-vkey vkey index)
			 (progn
			   (drop-vknode-value nextn stack)
			   (decf (table-count vktable))
			   (when (empty vktable)
			     (clear vktable))
			   t)
			 (rec nextn (1+ index))))))))
      (rec (make-vknode nil (table-root vktable)) 0))))

(defun vector-keyed-get (vktable vkey)
  "Internal recursion to walk tree and return value for vector
   key: vkey"
  (labels ((rec (table index)
	     (when table
	       (awhen (get-value table (aref vkey index))
		 (if (end-of-vkey vkey index)
		     (vknode-value it)
		     (rec (vknode-table it) (1+ index)))))))
    (rec (table-root vktable) 0)))

;;
;; ----------------------
;;
	   
(defmethod get-value ((table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-get table key))

(defmethod (setf get-value) (value (table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-put table key value))

(defmethod drop ((table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-rem table key))

(defmethod clear ((table vector-keyed-table))
  (setf (table-root table) 
	(make-instance 'hashed-table
		       :hash (make-hash-table :test #'eq :size 1000 :rehash-size 1.5 :rehash-threshold 0.7)))
  (setf (table-count table) 0)
  t)

(defmethod size-of ((table vector-keyed-table))
  (table-count table))

(defmethod storage-allocated ((table vector-keyed-table))
  ;; NOTE: TODO
  )

(defclass vector-keyed-table-iterator (iterator)
  ((reference :accessor reference :initarg :reference)
   (type :accessor iter-type :initarg :type)
   (last :accessor last-key :initform nil)
   (stack :accessor vkti-stack :initform nil)))

(defmethod get-iterator ((vktable vector-keyed-table) &key (type :pair))
  (make-instance 'vector-keyed-table-iterator
		 :reference vktable
		 :type type))

(defmethod initialize-instance :after ((iter vector-keyed-table-iterator) &rest initargs &key &allow-other-keys)
  (reset iter))

(defmacro mvpass2 (form)
  `(aif2 ,form
	 (values it t)
	 (values nil nil)))

(defmethod next ((iter vector-keyed-table-iterator))
  "Invariant: stack always contains a null, exhausted or intermediate table iterator"
  (with-slots (stack) iter
    (cond ((null stack)
	   (values nil nil))
	  ((or (null (car stack))
	       (not (next-p (car stack))))
	   (pop stack)
	   (pop (last-key iter))
	   (mvpass2 (next iter)))
	  (t 
	   (mvpass2 (vkti-next-value iter))))))

(defun vkti-next-value (iter)
  (with-slots (stack) iter
    (let* ((iterator (car stack))
	   (kvpair (next iterator))
	   (key (car kvpair))
	   (node (cdr kvpair)))
      (push key (last-key iter))
      (aif (vknode-table node)
	   (push (get-iterator it) stack)
	   (push nil stack))
      (aif (vknode-value node)
	   (values (extract-assoc-type (cons (list->array (reverse (last-key iter))) it) 
				       (iter-type iter))
		   t)
	   (mvpass2 (next iter))))))

(defmethod next-p ((iter vector-keyed-table-iterator))
  (with-slots (stack) iter
    (or (null stack)
	(every (lambda (table) (not (next-p table)))
	       stack))))

(defmethod drop-last ((iter vector-keyed-table-iterator))
  (awhen (last-key iter)
    (drop (reference iter) (list->array (reverse it)))
    (setf (last-key iter) nil)))

(defmethod reset ((iter vector-keyed-table-iterator))
  (setf (vkti-stack iter) 
	(list (get-iterator (table-root (reference iter))))))
 
(defmethod clear ((iter vector-keyed-table-iterator))
  (setf (vkti-stack iter) nil))

(defun test-drop (table value)
  (loop with iter = (get-iterator table :type :value)
     for val = (next iter)
     while val do
       (if (eq val value)
	   (drop-last iter)
	   (print val))))


;; Notes: add parameters for hashtable inits
(defvar *vechash-threshold* 10)
(defun-exported make-vechash (&key (depth nil))
  (declare (ignorable depth))
  (make-hash-table :test #'eq :size 1000 :rehash-size 1.5 :rehash-threshold 0.7))

;; Internal functions to reduce # of total hashtables using alists
(defun-exported vechash-int-get (key hash)
  (if (consp hash)
      (cdr (assoc key hash))
      (gethash key hash)))

(defun-exported vechash-int-put (key val hash)
  (cond ((and (consp hash) (assoc key hash)) ;; replace existing
	 (setf (cdr (assoc key hash)) val))
	((consp hash) ;; add new behind head to preserve validity of caller pointer to head of alist
	 (setf (cdr hash) (acons key val (cdr hash))))
	(t            ;; it's a hashtable
	 (setf (gethash key hash) val))))

(defmacro-exported vechash-upgrade-hash (vh-pair)
  `(if (and (consp (cdr ,vh-pair))
	    (> (length (cdr ,vh-pair)) *vechash-threshold*))
     (let ((new-hash (make-hash-table :test #'eq :size (* 2 *vechash-threshold*) :rehash-size 1.5 :rehash-threshold 0.7)))
       (dolist (a (cdr ,vh-pair))
	 (setf (gethash (car a) new-hash) (cdr a)))
       (setf (cdr ,vh-pair) new-hash))))

;; Depth limited vechash get
;;(defun-exported vechash-get-depth (vec-key vechash depth)
;;  (let ((last (min (1- (length vec-key)) depth)

;; Get from the hash tree, not depth limited
(defun-exported vechash-get-nodepth (vec-key vechash)
  (let ((last (1- (length vec-key))))
    (labels ((vhget (count ht)
	       (let* ((key (elt vec-key count))
		      (pdata (vechash-int-get key ht)))
		 (cond ((null pdata)       ;; early termination or no item
			nil) 
		       ((= count last)     ;; last index, pdata non-null return car (null or val)
			(car pdata))
		       ((null (cdr pdata)) ;; if not last index, but continuation is nil, terminate
			nil)
		       (t                  ;; otherwise, continue on...
			(vhget (1+ count) (cdr pdata)))))))
      (vhget 0 vechash))))

(defun-exported vechash-get (vec-key vechash)
;;  (if (car vechash)
;;      (vechash-get-depth vec-key (cdr vechash) (car vechash))
    (vechash-get-nodepth vec-key vechash))
  
		       
;; Destructive put
(defun-exported vechash-put (vec-key put-value vechash)
  (let ((last (1- (length vec-key))))
    (labels (;; End of sequence, put value in car of curr vechash entry
	     (vhset (key pdata ht)
	       (if (null pdata)
		 (progn (vechash-int-put key (cons put-value nil) ht) put-value)
		 (setf (car pdata) put-value)))
	     ;; Middle of sequence, extend current location by adding new ht & recurse
	     (vhextend (count pdata)
	       (let ((new-ht (acons -1 nil nil)))
		 (setf (cdr pdata) new-ht)
		 (vhput (1+ count) new-ht)))
	     ;; Recursive put, walk through sequence extending as necessary
	     (vhput (count ht)
	       (let* ((key (aref vec-key count))
		      (pdata (vechash-int-get key ht)))
		 (cond ((= count last) ;; done
			(vhset key pdata ht))
		       ((null pdata) ;; empty entry
			(let ((datum (cons nil nil)))
			  (vechash-int-put key datum ht)
			  (vhextend count datum)))
		       ((null (cdr pdata)) ;; value, no hashtable
			(vhextend count pdata))
		       (t  ;; recurse to next entry
			(vechash-upgrade-hash pdata)
			(vhput (1+ count) (cdr pdata)))))))
      (vhput 0 vechash))))

;; Map all elements in my funky vector hash
(defun-exported mapvechash (f vhash)
  (if (consp vhash)
      (dolist (asoc vhash)
	(let ((pair (cdr asoc)))
	  (when (car pair) (funcall f (car pair)))
	  (when (cdr pair) (mapvechash f (cdr pair)))))
    (loop for pair being the hash-value of vhash do 
	  (when (car pair) (funcall f (car pair)))
	  (when (cdr pair) (mapvechash f (cdr pair))))))
 