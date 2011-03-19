;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.lisp
;;;; Purpose:       File related utility functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004

(in-package :stdutils)

;; ---------------------------------
;; Basic list manipulation functions

(eval-when (eval compile load)
  (proclaim '(optimize speed (safety 1) (debug 1))))

(defun-exported mklist (obj)
  "Make into list if atom"
  (if (listp obj) obj (list obj)))

(defun-exported same-length (a b)
  (eq (length a) (length b)))

;;
;; Various reordering, merging and other neat list operations
;; 

(defun-exported shuffle (x y)
  "Mix x and y element for element into a single list"
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y) 
                  (shuffle (cdr x) (cdr y))))))

(defun-exported unshuffle (n x)
  "Create n lists, each getting the
   successive element of x.  Returns
   a list of lists.  Retains order."
  (let ((lists (repeat nil n)))
    (loop 
      for elt in x and
      count from 0 do
      (push elt (nth (mod count n) lists)))
    (mapcar #'nreverse lists)))

(defun-exported repeat (e n)
  "repeat element e n times into a list"
  (if (<= n 0) nil
    (cons e (repeat e (1- n)))))

(defun-exported repeat-fn (fn n)
  "repeat thunk fn n times, results are
   returned in a list, car is the result
   of the first call"
;;  (declare (:explain :tailmerging))
  (labels ((rec (list count)
		(if (= count n)
		    list
		  (rec (cons (funcall fn) list)
		       (1+ count)))))
    (when (> n 0)
      (nreverse (rec nil 0)))))

(defmacro-exported pushlast (value place)
  `(if (consp ,place)
       (setf (cdr (last ,place)) 
	     (cons ,value nil))
       (setf ,place (cons ,value nil))))
       

(defmacro-exported poplast (place)
  "Remove the last element from the list and return it"
  `(let ((list ,place))
     (if (> (length list) 1)
	 (let ((newlast (last list 2)))
	   (let ((oldlast (cadr newlast)))
	     (setf (cdr newlast) nil)
	     oldlast))
	 (progn 
	   (setf ,place nil)
	   (when list 
	     (car list))))))

(defmacro-exported rotate (place)
  `(push (poplast ,place) ,place))

(defun-exported group (list num)
  "Returns a list of sub-lists, each sublist being the
   next 'num' of the source list.  Lists that are not
   multiples of 'num' will have a truncated final list."
  (labels ((grp (list sublist subcount new)
	     (cond ((null list)
		    (nreverse (cons (nreverse sublist) new)))
		   ((= subcount num)
		    (grp (cdr list) 
			 (list (car list)) 
			 1 
			 (cons (nreverse sublist) new)))
		   (t 
		    (grp (cdr list) 
			 (cons (car list) sublist) 
			 (1+ subcount) 
			 new)))))
    (when (>= num 1) 
      (grp (cdr list) 
	   (list (car list)) 
	   1 
	   nil))))

(defun-exported gather (&rest columns)
  "Gathers top elements from each column in columns
   and groups them in order, like inverting an array"
  (apply #'mapcar (cons #'list columns)))

(defun-exported gather-pairs (list1 list2)
  (mapcar #'cons list1 list2))

(defun-exported distribute (rows)
  "distributes each element of each row into a separate contiguous list,
   like inverting an array"
  (labels ((dist-row (row cols)
		     (mapcar #'cons row cols))
	   (rec (rows columns)
		(cond ((null rows)
		       (mapcar #'nreverse columns))
		      (t (rec (cdr rows) (dist-row (car rows) columns))))))
    (rec (cdr rows) (mapcar #'list (car rows)))))

(defun-exported pairs (list)
  "Take each set of two elements in the list and make a list one half
   the size containing a pair of each two items"
  (labels ((pairing (list accum num)
		    (cond ((null list)
			   (nreverse accum))
			  ((and (consp list) (null (cdr list)))
			   (nreverse (cons (cons (car list) nil) accum)))
			  (t (pairing (cddr list) 
				      (cons (cons (car list) (cadr list)) accum)
				      (- num 2))))))
    (pairing list nil (length list))))

(defun-exported cons->list (list)
  "Convert a list of cons cells to a list of two-element lists, i.e. (a . b) => (a b)"
  (mapcar #'(lambda (cons)
	      (list (car cons) (cdr cons)))
	  list))

(defun-exported split-list (item list &key (key #'identity) (test #'eq) (discard nil))
  "Walks list to first match of item and returns two fresh 
   lists; one up to the item and the remainder including the item."
  (labels ((rec (new orig)
		(cond ((null orig)
		       (values (nreverse new) nil))
		      ((funcall test item (funcall key (car orig)))
		       (values (nreverse new) (if discard (cdr orig) orig)))
		      (t (rec (cons (car orig) new) (cdr orig))))))
    (rec nil list)))
      

(defun-exported break-list (item list &key (key #'identity) (test #'eq) (discard nil))
  "Destructive version of split-list"
  (labels ((rec (last rest)
		(cond ((null rest)
		       (values list nil))
		      ((funcall test item (funcall key (car rest)))
		       (if last 
			   (progn
			     (setf (cdr last) nil)
			     (values list (if discard (cdr rest) rest)))
			 (progn
			    (values nil (if discard (cdr rest) rest)))))
		      (t (rec rest (cdr rest))))))
    (rec nil list)))


(defun-exported unique-pairs (list)
  "Produce all unique pairings of the provided list"
  (if (null list) nil
    (nconc (combinations (list (car list)) (cdr list))
	 (unique-pairs (cdr list)))))

(defun-exported combinations (&rest lists)
  "Create a list for each combination formed of
   an element from each provided list.  ie
   (list-combinations '(a b c) '(d e)) =>
   '((a d) (a e) (b d) (b e) (c d) (c e))"
  (flet ((expand-combos (list combos)
	   (loop for elt in list
	         nconcing 
		 (loop for combo in combos
		   collect (cons elt combo)))))
    (cond ((null lists) 
	   nil)
	  ((>= 1 (length lists))
	   (mapcar #'mklist (car lists)))
	  (t (expand-combos (car lists) (apply #'combinations (cdr lists)))))))

(defun-exported distinct (list &key (test #'eql) (key #'identity))
  "Create a reduced list which groups all equal terms in the original into sublists
   (1 2 2 3 3 3 4) => ((1) (2 2) (3 3 3) (4))"
  (ndistinct (copy-list list) :test test :key key))

(defun-exported ndistinct (list &key (test #'eql) (key #'identity))
  "Like distinct, but performs a destructive sort on the original list"
  (labels ((rec (current accum rest)
	     (cond ((null rest)
		    (cons current accum))
		   ((funcall test 
			     (funcall key (first current))
			     (funcall key (first rest)))
		    (rec (cons (first rest) current) accum (rest rest)))
		   (t (rec (list (first rest)) (cons current accum) (rest rest))))))
    (when (> (length list) 0)
      (let ((sorted (sort list #'< :key key)))
	(rec (list (first sorted)) nil (rest sorted))))))

;;
;; Filtering
;;


(defun-exported select-if (ff lst &key key)
  "Extract only the elements from the lst that satisfy the
   predicate ff"
  (labels 
      ((rfilt (lst acc)
	 (if (null lst)
	     (nreverse acc)
	     (let* ((elt (car lst))
		    (cmp (if key 
			     (funcall key elt)
			     elt)))
	   (if (funcall ff cmp)
	       (rfilt (cdr lst) (cons elt acc))
	       (rfilt (cdr lst) acc))))))
    (rfilt lst nil)))

(defmacro-exported filter-if (ff lst &key key)
  "Remove the elements from the list that satisfy predicate.
   Filter here is in the signal processing sense, filter away"
  `(select-if #'(lambda (x) (not (funcall ,ff ,(if key `(funcall ,key x) 'x)))) ,lst))

(defun-exported remove-nulls (list)
  (filter-if #'null list))

(defun-exported nmerge-duplicates (func list &key (test #'eq))
  "Destructively walks the list and when test applies between the head and any
   remaining element it applies func to the two elements, makes that the newly
   accumulated value (replacing all instances of matches to the head) and then
   compares the original element again to the remainder of the list.  The choice
   of 'head' comparison with the original head value is for numerical quantities, ie:
   (nmerge-duplicates #'+ '(1 2 3 1 2 3 4 5 5 4) :test #'=) ==> '(2 4 6 8 10)"
  (labels ((walk (list)
		 (unless (null list) 
		   (setf (car list) (merge-elt list (car list) (car list) (cdr list)))
		   (walk (cdr list))))
	   (merge-elt (cell oelt celt list)
		      (cond ((null list) 
			     celt)
			    ((funcall test oelt (car list))
			     (setf (cdr cell) (cdr list))
			     (merge-elt cell
					oelt
					(funcall func celt (car list))
					(cdr list)))
			    (t (merge-elt (cdr cell)
					  oelt
					  celt
					  (cdr list))))))
    (walk list)
    list))

;;(gen-list (newlist remlist)
;;		     (let ((merged-list 
;;			    (merge-head (car remlist) 
;;					remlist
;;					(cdr remlist))))
;;		       (gen-list (cons (car merged-list) newlist) 
;;				 (cdr remlist))))
;;	   (merge-head (merge-elt head list)
;;		       (if (funcall test merge-elt (car list))
;;			   (let ((merged (funcall func merge-elt (car list)))
;;				  (merge-elts 
			       

(defun-exported safe-subseq (sequence start &optional end)
  (let ((len (length sequence))
	(st (if (< start 0) 0 start)))
    (subseq sequence st (when end (min len end)))))

;;
;; Finding
;; 
	  
(defun-exported rfind-if (fn tree)
  "Find first atom in tree on which
   fn returns true"
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))

(defun-exported rfind-node-if (fn tree)
  "Recurse cons-based tree structure until a matching subtree is found"
  (cond ((null tree) nil)
	((funcall fn tree) tree)
	((atom tree) nil)
	((consp tree)
	 (or (rfind-node-if fn (car tree))
	     (rfind-node-if fn (cdr tree))))
	(t nil)))

(defun-exported longer (x y)
  "Is x longer than y?"
  (labels ((compare (x y)
             (and (consp x) 
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun-exported find2 (fn lst)
  "returns two values, the first
   element in lst on which fn returns
   a non-null value and the value
   so returned."
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

;;
;; Trees
;;
 
(defun-exported filter (fn lst)
  "Extract elements on which fn returns true"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun-exported before (x y lst &key (test #'eql))
  "Returns true if an x occurs before the first y in lst"
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun-exported after (x y lst &key (test #'eql))
  "Returns true if any y occurs before the first x in lst"
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun-exported most (fn lst)
  "Get the highest scoring element from list using
   fn as the scoring routine.  If there are multiple
   elements with the highest score this gets the first."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun-exported best (fn lst)
  "Get the best element from list using
   fn as a comparison of list elements.
   fn returns nul if first object fails
   or the object if it wins."
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun-exported mostn (fn lst)
  "Get the highest scoring elements from list using
   fn as the scoring routine."
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;
;; Sets
;;

(defun-exported disjunction (set1 set2)
  "Compute the non-intersecting elements of set1 and set2"
  (nunion (set-difference set1 set2)
	 (set-difference set2 set1)))

(defun-exported set-equal (set1 set2)
  (and (every #'(lambda (el1) (member el1 set2)) set1)
       (every #'(lambda (el2) (member el2 set1)) set2)))


(defun-exported remove-sublist (list sublist &key (test #'eq) (split nil))
  "Finds and removes the first occurances of sublist in list"
  (let ((start nil))
    (labels ((find-start (position rest)
			 (cond ((null rest)
				nil)
			       ((funcall test (car sublist) 
					 (car rest))
				(if (match-it rest sublist)
				    (setf start position)
				  (find-start (1+ position) (cdr rest))))
			       (t (find-start (1+ position) (cdr rest)))))
	     (match-it (list1 list2)
		       (cond ((null list2)
			      t)
			     ((null list1)
			      nil)
			     ((not (funcall test (car list1) (car list2)))
			      nil)
			     (t (match-it (cdr list1) (cdr list2))))))
      (find-start 0 list)
      (if start
	  (let ((before (subseq list 0 start))
		(after (subseq list (+ start (length sublist)))))
	    (if split
		(cons before after)
	      (append before after)))
	list))))

;;
;; Misc
;;


(defun-exported flatten (x)
  "Turn a cons tree in to a list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil))) 

(defun-exported flatten1 (x)
  "Flatten a list by removing only one level"
  (mapcan #'identity x))

(defun-exported prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree) 
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun-exported duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test)) 
          :test test))

(defun-exported split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;; alist operators: Avoid evil car/cdrs after assoc

(defun-exported assoc-get (key list &optional (test #'eq))
  (let ((it (assoc key list :test test)))
    (if it 
	(values (cdr it) t)
      (values nil nil))))

(defun-exported assoc-put (key value list &optional (test #'eq))
  (let ((it (assoc key list :test test)))
    (if it 
	(setf (cdr it) value)
      nil)))

(defsetf assoc-get assoc-put)

(defmacro-exported assoc-setf (place key value &optional (test #'eq))
  (with-gensyms (list it)
    `(let* ((,list ,place)
	    (,it (assoc ,key ,list :test ,test)))
       (if ,it
	   (setf (cdr ,it) ,value)
	   (setf ,place (acons ,key ,value ,list))))))

(defun-exported assoc-find (value list &optional (test #'eq))
  (find value list :test test))

(defun-exported assoc-rem (value list)
  (remove value list :key #'car))

;; NOTE: duplicate place references in these macros, fix?
(defmacro-exported assoc-del (value place)
  `(setf ,place (assoc-rem ,value ,place)))

(defmacro-exported apush (key value place)
  `(push (cons ,key ,value) ,place))

(defmacro-exported apushnew (key value place)
  `(aif (find ,key ,place :key #'car :test #'equal)
	(progn (setf (cdr it) ,value)
	       ,place)
	(apush ,key ,value ,place)))


;; Position related stuff

(defun-exported collect-offsets (list)
  (let ((offsets nil))
    (loop 
      for offset from 0
      for elt in list do
      (unless (null elt)
	(push offset offsets)))
    (nreverse offsets)))

(defun list-or-struct (list count)
  (cond ((>= count 3) :list)
	((null list) nil)
	((atom list) (type-of list))
	((< count 3)
	 (let ((rest (list-or-struct (cdr list) (1+ count))))
	   (cond ((eq rest :list) :list)
		 (t (cons (list-structure (car list))
			  rest)))))))

(defun-exported list-structure (list)
    (cond ((null list) nil)
	  ((atom list) (type-of list))
	  ((consp list) (list-or-struct list 0))))

;; -------------------------------------------------------
;; LIST RECURSION GENERATION

(defun-exported lrec (rec &optional base)
  "Guides a list recursion process and takes 
   recursive functions of x = car and fn = recursive call 
   the terminal end of list case returns the value of base.
   Beware of non-tail recursive implementation"
  (labels ((self (lst)
		 (if (null lst)
		     (if (functionp base)
			 (funcall base)
		       base)
		   (funcall rec (car lst)
			    #'(lambda ()
				(self (cdr lst)))))))
    #'self))

;; ie: (defun our-length (lst) 
;;        (lrec #'(lambda (x f) (1+ (funcall f))) 0))

(defmacro-exported alrec (rec &optional base)
  "Anaphoric recursion templates"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (symbol-macrolet ((rec (funcall ,gfn)))
		 ,rec))
	   ,base)))

;; ie:   (alrec (1+ rec) 0))
;; and:  (alrec (and (oddp it) rec) t))


;; See usage of on-cdrs below in set ops
(defmacro-exported on-cdrs (rec base &rest lsts)
  "Utility to defun anaphoric recursion functions over lists"
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))


;; -------------------------------------------------------
;; TREE RECURSION GENERATION

(defun-exported trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		   base)
	       (funcall rec tree
			#'(lambda ()
			    (self (car tree)))
			#'(lambda ()
			    (if (cdr tree)
				(self (cdr tree))))))))
    #'self))

(eval-when (compile eval load)
  (export '(left right)))

(defmacro-exported atrec (rec &optional (base 'it))
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (declare (ignorable it))
	       (symbol-macrolet ((left (funcall ,lfn))
				 (right (funcall ,rfn)))
		 ,rec))
	   #'(lambda (it) (declare (ignorable it)) ,base))))

(defmacro-exported on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

;; -------------------------------------------------------
;; List set functions using on-cdrs

(defun-exported unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun-exported intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun-exported set-differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun-exported maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
	       (values (max mx it) (min mn it)))
	     (values (car args) (car args))
	     (cdr args))))

(defun-exported list-max (list)
  (on-cdrs (max rec it) 0 list))

;; ------------------------
;; Tree recursive operations

(defun-exported nflatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun-exported count-leaves (tree &key (count-last-nil nil))
  (on-trees (+ left (or right (if count-last-nil 1 0))) 1 tree))

(defun-exported localize-expression (exp &key (package *package*) exceptions)
  (on-trees (cons left right) (localize-symbol it :package package :exceptions exceptions) exp))


;; 
;; Really odd functions
;; 

(defun-exported explode (sym)
  "Turn a symbol into a list of the constitutent characters of its name"
  (map 'list #'(lambda (c)
                 (intern (make-string 1 
                                      :initial-element c)))
             (symbol-name sym)))


(defun-exported append-sublists (list)
  "Takes a list of lists and appends all sublists"
  (let ((results (car list)))
    (dolist (elem (cdr list) results)
      (setq results (append results elem)))))


(defmacro-exported multiple-intersection (passed-lists &rest rest)
  "Takes the intersection of one or more lists.  If one list is passed, returns that list.
Keylist accepts :key, :test, and :test-not"
  (let ((inter (gensym))
	(lists (gensym))
	(lst (gensym)))
    `(let* ((,lists ,passed-lists)
	    (,inter (car ,lists)))
       (loop for ,lst in (cdr ,lists) do
	 (setq ,inter (intersection ,inter ,lst ,@rest)))
       ,inter)))

;; -------------------------------------------------------
;; RECURSIVE ARRAY WALKING FUNCTION BUILDER

;; start index
;; end index
;; compute next value: 

(defun max-array-tr (array start)
  (labels ((self (accum index)
		 (if (= (length array) index)
		     accum
		   (self (+ accum (aref array index))
			 (1+ index)))))
    (self 0 start)))
	

(defun-exported arec (rec &optional base)
  "Walks an array as in lrec, base case is end of array"
  (labels ((self (ary &optional (index 0))
		 (if (= (length ary) index)
		     (if (functionp base)
			 (funcall base)
		       base)
		   (funcall rec (aref ary index)
			    #'(lambda ()
				(self ary (1+ index)))))))
    #'self))
   
;; TODO: Can make this tail recursive?
(defmacro-exported aarec (rec &optional base)
  "Anaphoric recursion template over arrays"
  (let ((gfn (gensym)))
    `(arec #'(lambda (it ,gfn)
	       (symbol-macrolet ((rec (funcall ,gfn)))
		 ,rec))
	   ,base)))

(defmacro-exported on-array (rec base &rest arrays)
  "Utility to build functions that walk arrays"
  `(funcall (aarec ,rec #'(lambda () ,base)) ,@arrays))

(defun-exported sum-array (array &optional (start 0))
  (on-array (+ rec it) 0 array start))

(defun-exported max-array (array &optional (start 0))
  (on-array (max rec it) 0 array start))

;;
;; Simple conversions
;;

(defun-exported array->list (array &aux list)
  (loop for elt across array do
    (push elt list))
  (nreverse list))

(defun-exported list->array (list &key (adjustable nil) (type t))
  (make-array (length list) :initial-contents list :adjustable adjustable
	      :element-type type))

;; -------------------------------
;; Funky sorting
;; -------------------------------

(defun-exported sort-b-according-to-a (alist blist predicate &optional (key #'first))
  "Sorts blist according to the reordering of alist and returns
   the newly ordered blist.  Non-destructive to alist and blist"
  (second
   (apply #'gather 
	  (sort (distribute (list alist blist))
		predicate :key key))))

(defun-exported sort-ordering-by (order elements key)
  "Simple n squared order b according to a"
  (loop for val in order
     for elt = (find val elements :key key)
     collect elt))

;; ---------------
;; Mapping
;; ---------------

(defun-exported map-all-combinations (function &rest lists)
  "Applies function to every combination of the elements of the lists provided. (function) should recieve a single list as a parameter.  Don't count on the order of application. Beware! This is an n^(huge) power function! Use carefully."
  (labels ((loop-rest (tail function remaining-front)
		      (if (null remaining-front)
			  (funcall function tail)
			(loop for element in (car (last remaining-front)) do 
			  (let ((x (butlast remaining-front)))
			    (loop-rest (cons element tail) function x))))))
    (loop-rest nil function lists)))


;; ====================
;;  Compose operations
;; ====================

(defparameter *string-ops*
  '((trim string-trim-whitespace first)
    (split ppcre:split last)
    (join join-op last)
    (map mapcar fn)))

(defun join-op (interp list)
  (apply #'concatenate 'string 
	 (shuffle list
		  (repeat interp (1- (length list))))))

(defun lookup-oprec (name)
  (assoc name *string-ops*))

(defun lookup-op (name)
  (awhen (lookup-oprec name)
    (second it)))

(defun build-sop (input-expr args)
  (if (null args)
      input-expr
      (expand-expr (first args) 
		   (build-sop input-expr (rest args)))))

(defun expand-expr (template argument)
  (when (symbolp template)
    (return-from expand-expr 
      (aif (lookup-op template)
	   `(,(lookup-op template) ,argument)
	   (if (symbol-function template)
	       `(,template ,argument)
	       (error "Unrecognized function argument: ~A" template)))))
  (dbind (name func pos) (lookup-oprec (first template))
	 (cond ((and (not name) (symbol-function (first template)))
                `(,(first template) ,@(rest template) ,argument))
               ((eq pos 'first)
		`(,func ,argument ,@(rest template)))
	       ((eq pos 'last)
		`(,func ,@(rest template) ,argument))
	       ((eq pos 'fn)
		(let ((fn (lookup-op (second template))))
		  (cond (fn 
			 `(,func #',fn ,argument))
			((symbol-function (second template))
			 `(,func #',(second template) ,argument)))))
	       (t (error "Operation not recognized or function not defined in sop: ~A" template)))))

(defmacro sop (string &rest args)
  (build-sop string (reverse args)))
