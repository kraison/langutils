;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hashutils.lisp
;;;; Purpose:       File related utility functions
;;;; Programmer:    Push Singh (plus a few mods by Ian Eslick)
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

(defmacro-exported hash (&key (test nil) (rehash-size 10) (keyvals nil) (hash nil))
  ;--------------------------
  ; Return a new hash table.
  ;--------------------------
  `(progn
     (let ((h (make-hash-table ,@(when test (list :test test)) ,@(list :rehash-size rehash-size))))
       (when ,keyvals
	 (hash-populate h ,keyvals))
       (when ,hash
	 (do-hash (k v ,hash) (hash-put h k v)))
       h)))

(defmacro-exported hash-put (hash key value)
  ;-----------------------------
  ; Insert KEY-VALUE into HASH.
  ; Return HASH.
  ;-----------------------------
  `(progn
     (setf (gethash ,key ,hash) ,value)
     ,hash))

(defmacro-exported hash-populate (hash kvlist)
  ;-----------------------------
  ; Insert KEY, VALUE elements 
  ;      from 2-element KVLIST
  ; Return Hash
  ; ----------------------------
  `(progn
     (dolist (kv ,kvlist)
       (hash-put ,hash (first kv) (second kv)))
     ,hash))

(defmacro-exported hash-mapulate (hash f values)
  ;-----------------------------
  ; Insert KEY, VALUE pairs from all 
  ;    elements of VALUES where f is 
  ;    mapped over values to create keys
  ; Return Hash
  ; ----------------------------
  `(progn
     (dolist (v ,values)
       (hash-put ,hash (,f v) v))
     hash))
  
(defmacro-exported hash-mapulate-value (hash f keys)
  `(let ((h ,hash))
     (dolist (k ,keys h)
       (hash-put h k (,f k)))))

(defmacro-exported hash-get (hash key)
  ;------------------------------
  ; Return value of KEY in HASH.
  ;------------------------------
  `(gethash ,key ,hash))

(defmacro-exported hash-rem (hash key)
  `(remhash ,key ,hash))

(defmacro-exported hash-clear (hash)
  `(clrhash ,hash))

(defmacro-exported hash-clr (hash)
  `(clrhash ,hash))

(defmacro-exported dohash ((elem hash) &body body)
  ;-------------------------------------------------------
  ; Iterate over elements of HASH.  On each iteration
  ; evaluate BODY with ELEM bound to the current element.
  ;-------------------------------------------------------
  `(maphash
    (lambda (k v)
      (declare (ignore k))
      (let ((,elem v))
        ,@body))
    ,hash))

(defmacro-exported do-hash ((k v hash) &body body)
  ;-------------------------------------------------------
  ; Iterate over elements of HASH.  On each iteration
  ; evaluate BODY with ELEM bound to the current element.
  ;-------------------------------------------------------
  `(maphash
    (lambda (,k ,v)
      ,@body)
    ,hash))

(defun-exported hash-values (hash)
  ;--------------------------------
  ; Return list of values in HASH.
  ;--------------------------------
  (let ((lst nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v lst))
             hash)
    lst))

(defun-exported hash-keys (hash)
  ;------------------------------
  ; Return list of keys in HASH.
  ;------------------------------
  (let ((lst nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k lst))
             hash)
    lst))

(defun-exported hash-items (hash)
  "Return the alist with the same data as the hash-table.
Like alist->hash-table but the first element is not the test: '(key0 . val0) (key1 . val1)."
  (declare (hash-table hash))
  (let ((lst nil))
    (maphash (lambda (k v)
	       (push (cons k v) lst))
	     hash)
    lst))

(defun-exported hash-empty? (hash)
  ;-------------------------
  ; True iff HASH is empty.
  ;-------------------------
  (null (hash-keys hash)))

(defmacro-exported has-key? (hash key)
  ;-------------------------------
  ; True iff hash table uses KEY.
  ;-------------------------------
  `(multiple-value-bind (value has-key) (gethash ,key ,hash)
     (declare (ignore value))
     has-key))

(defun-exported hash-push (hash key value)
  ;--------------------------------------------------------------
  ; Push VALUE onto entry indexed by KEY in HASH.
  ; If HASH does not have KEY, map KEY to list containing VALUE.
  ;--------------------------------------------------------------
  (if (has-key? hash key)
    (hash-put hash key (cons value (hash-get hash key)))
    (hash-put hash key (list value))))

(defun-exported hash-eq? (h1 h2)
  (every #'(lambda (x) x)
         (mapcar (lambda (k) (eq (hash-get h1 k) (hash-get h2 k)))
		 (hash-keys h1))))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Ian added treating hashes as sets for
;; large set operations ~100+

;; NOTE: Hashset rep is that the keys map to themselves, quick
;; way to determine membership
;; NOTE: Better way to do this?  Might hurt gc if hash index points
;; to hash value...

;; Returns a new hashset that is a union of the two
(defmacro-exported hashset-union (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1) :hash ,hset1)))
     (maphash #'(lambda (k v)
		  (hash-put h k v))
	      ,hset2)
     h))

;; Returns a new hashset that is a union of the two (destructive version)
(defmacro-exported nhashset-union (hset1 hset2)
  `(progn
    (maphash #'(lambda (k v)
		 (hash-put ,hset1 k v))
	     ,hset2)
     ,hset1))

;; Returns a new hashset that is the intersection
(defmacro-exported hashset-intersection (hset1 hset2)
  `(let* ((h (hash :test (hash-table-test ,hset1)))
	  (h1 (if (> (hash-table-size ,hset1) (hash-table-size ,hset2)) ,hset1 ,hset2))
	  (h2 (if (eq ,hset1 h1) ,hset2 ,hset1)))
     (maphash #'(lambda (k v)
		  (if (hash-get h1 k)
		      (hash-put h k v)))
	      h2)
     h))
  
(defmacro-exported hashset-difference (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1))))
     (maphash #'(lambda (k v)
		  (unless (hash-get ,hset2 k) 
		    (hash-put h k v)))
	      ,hset1)
     h))

(defun-exported hashset-populate (hset list &key (key #'identity))
  (dolist (elt list hset)
    (let ((hkey (funcall key elt)))
      (hash-put hset hkey hkey))))


;; ==============================
;; Save and restore a hash file
;; ==============================

;;(defun hash-write-to-file (hash file)
;;  (with-open-file (s file :direction :output :if-exists :supercede)
;;    (excl:fasl-write hash s)))

;;(defun hash-from-file (file)
;;  (excl:fasl-read file))

;; Hash Functions -- added by Bo Morgan <bo@mit.edu> (my brain is still case sensitive and object oriented)

(defun-exported Hash-get_length (this)
  (hash-table-count this))

(defun-exported Hash-get_test (this)
  (hash-table-test this))

(defun-exported Hash-get_rehash_size (this)
  (hash-table-rehash-size this))

(defun-exported Hash-new_copy (this)
  (hash :test        (Hash-get_test this)
	:rehash-size (Hash-get_rehash_size this)
	:hash        this))

(defun-exported Hash-sure_get (this key)
  (multiple-value-bind (value success)
      (hash-get this key)
    (if (eq success nil)
	(error (format t "Hash-sure_get Error: Key not found [~A] in ~A~%" key (hash-keys this))))
    value))

(defun-exported Hash-sure_set (this key value)
  (multiple-value-bind (value success)
      (hash-get this key)
    (if (eq success nil)
	(error (format t "Hash-sure_set Error: Key not found [~A] in ~A~%" key (hash-keys this))))
    (setf (hash-get this key) value)))

(defun-exported Hash-contains (this key)
  (has-key? this key))

(defun-exported update-hash (hash key value 
				  &optional (fn #'(lambda (x y) 
						    (declare (ignore x)) y)))
  (multiple-value-bind (old-value found)
      (gethash key hash)
    (if found
	(setf (gethash key hash) (funcall fn old-value value))
	(setf (gethash key hash) value)))
  hash)

(defun-exported update-hash-list (hash alist 
				       &optional (fn #'(lambda (x y) 
							 (declare (ignore x)) y)))
  (progn
    (map 'nil #'(lambda (x) (update-hash hash (car x) (cdr x) fn))
	 alist)
    hash))

(defun-exported update-hash-hash(hash hash2
				      &optional (fn #'(lambda (x y) 
							(declare (ignore x)) y)))
  (with-hash-table-iterator (iter hash2)
    (loop (multiple-value-bind (re kk vv) (iter)
	    (unless re (return))
	    (update-hash hash kk vv fn)))))