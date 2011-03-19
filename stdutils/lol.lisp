;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!


(in-package :stdutils)


(defun-exported mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun-exported symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun-exported group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defun-exported flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

(defun-exported fact (x)
  (if (= x 0)
    1
    (* x (fact (- x 1)))))

(defun-exported choose (n r)
  (/ (fact n)
     (fact (- n r))
     (fact r)))


(eval-when (:compile-toplevel :load-toplevel)
  (defun-exported g!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "G!"
		  :start1 0
		  :end1 2)))

  (defun-exported o!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "O!"
		  :start1 0
		  :end1 2)))

  (defun-exported o!-symbol-to-g!-symbol (s)
    (symb "G!"
	  (subseq (symbol-name s) 2))))


(defmacro-exported defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
               syms)
         ,@body))))

(defmacro-exported defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro defmacro!-exported (name pattern &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defmacro! ,name ,pattern ,@body)))


;; Graham's alambda
(defmacro-exported alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; Graham's aif
(defmacro-exported aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


;; Nestable suggestion from Daniel Herring

(eval-when (:compile-toplevel :load-toplevel)
  (defun-exported |#"-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars (state 'normal) (depth 1))
      (loop do
	   (let ((curr (read-char stream)))
	     (cond ((eq state 'normal)
		    (cond ((char= curr #\#)
			   (push #\# chars)
			   (setq state 'read-sharp))
			  ((char= curr #\")
			   (setq state 'read-quote))
			  (t
			   (push curr chars))))
		   ((eq state 'read-sharp)
		    (cond ((char= curr #\")
			   (push #\" chars)
			   (incf depth)
			   (setq state 'normal))
			  (t
			   (push curr chars)
			   (setq state 'normal))))
		   ((eq state 'read-quote)
		    (cond ((char= curr #\#)
			   (decf depth)
			   (if (zerop depth) (return))
			   (push #\" chars)
			   (push #\# chars)
			   (setq state 'normal))
			  (t
			   (push #\" chars)
			   (if (char= curr #\")
			       (setq state 'read-quote)
			       (progn
				 (push curr chars)
				 (setq state 'normal)))))))))
      (coerce (nreverse chars) 'string)))

  (set-dispatch-macro-character
   #\# #\" #'|#"-reader|)


  (defun-exported |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
		 (read-char stream)))
	  ((char= #\newline curr))
	(push curr chars))
      (let ((pattern (nreverse chars))
	    output)
	(labels ((match (pos chars)
		   (if (null chars)
		       pos
		       (if (char= (nth pos pattern) (car chars))
			   (match (1+ pos) (cdr chars))
			   (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
	  (do (curr
	       (pos 0))
	      ((= pos (length pattern)))
	    (setf curr (read-char stream)
		  pos (match pos (list curr)))
	    (push curr output))
	  (coerce
	   (nreverse
            (nthcdr (length pattern) output))
	   'string)))))

  (set-dispatch-macro-character
   #\# #\> #'|#>-reader|)


  (defun-exported segment-reader (stream ch n)
    (if (> n 0)
	(let ((chars))
	  (do ((curr (read-char stream)
		     (read-char stream)))
	      ((char= ch curr))
	    (push curr chars))
	  (cons (coerce (nreverse chars) 'string)
		(segment-reader stream ch (- n 1))))))

  #+cl-ppcre
  (defmacro!-exported match-mode-ppcre-lambda-form (o!args o!mods)
    ``(lambda (,',g!str)
	(cl-ppcre:scan
	 ,(if (zerop (length ,g!mods))
	      (car ,g!args)
	      (format nil "(?~a)~a" ,g!mods (car ,g!args)))
	 ,',g!str)))

  #+cl-ppcre
  (defmacro!-exported subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
	(cl-ppcre:regex-replace-all
	 ,(car ,g!args)
	 ,',g!str
	 ,(cadr ,g!args))))

  #+cl-ppcre
  (defun-exported |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
	((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
	  (segment-reader stream
			  (read-char stream)
			  1)
	  (coerce (loop for c = (read-char stream)
		     while (alpha-char-p c)
		     collect c
		     finally (unread-char c stream))
		  'string)))
	((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
	  (segment-reader stream
			  (read-char stream)
			  2)))
	(t (error "Unknown #~~ mode character")))))

  #+cl-ppcre
  (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

  (defun-exported |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
		 collect (symb 'a i))
       ,(funcall
	 (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character
    #\# #\` #'|#`-reader|))


(defmacro!-exported dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                         g!args
                         `(cdr ,g!args)))))
           ds))))

(defmacro-exported alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro-exported alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defun-exported let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

(defmacro-exported pandoriclet (letargs &rest body)
  (let ((letargs (cons
                   '(this)
                   (let-binding-transform
                     letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply this args))))))

(defun-exported pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun-exported pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym))))


(declaim (inline get-pandoric))

(defun-exported get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro-exported with-pandoric (syms box &rest body)
  (let ((g!box (gensym "box")))
    `(let ((,g!box ,box))
       (declare (ignorable ,g!box))
       (symbol-macrolet
         (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                    syms))
         ,@body))))

(defun-exported pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

(defmacro-exported pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))

(defmacro-exported plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)
                  ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)
                  ,(pandoriclet-set pargs))
                (t (&rest args)
                  (apply this args)))))))

(defvar pandoric-eval-tunnel)

(defmacro-exported pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars pandoric-eval-tunnel
              ,,expr))))


;; Chapter 7

(eval-when (:compile-toplevel :load-toplevel)
  (set-dispatch-macro-character #\# #\f
   (lambda (stream sub-char numarg)
     (declare (ignore stream sub-char))
     (setq numarg (or numarg 3))
     (unless (<= numarg 3)
       (error "Bad value for #f: ~a" numarg))
     `(declare (optimize (speed ,numarg)
                         (safety ,(- 3 numarg)))))))

(defmacro-exported fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro-exported safe-progn (&rest body)
  `(locally #0f ,@body))

(defun-exported fformat (&rest all)
  (apply #'format all))

(define-compiler-macro fformat
                       (&whole form
                        stream fmt &rest args)
  (if (constantp fmt)
    (if stream
      `(funcall (formatter ,fmt)
         ,stream ,@args)
      (let ((g!stream (gensym "stream")))
        `(with-output-to-string (,g!stream)
           (funcall (formatter ,fmt)
             ,g!stream ,@args))))
    form))


(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun-exported make-tlist () (cons nil nil))
(defun-exported tlist-left (tl) (caar tl))
(defun-exported tlist-right (tl) (cadr tl))
(defun-exported tlist-empty-p (tl) (null (car tl)))



(declaim (inline tlist-add-left
                 tlist-add-right))

(defun-exported tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun-exported tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))


(declaim (inline tlist-rem-left))

(defun-exported tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))


(declaim (inline tlist-update))

(defun-exported tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defun-exported build-batcher-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
      (let ((q (ash 1 (- tee 1)))
            (r 0)
            (d p))
        (loop while (> d 0) do
          (loop for i from 0 to (- n d 1) do
            (if (= (logand i p) r)
              (push (list i (+ i d))
                    network)))
          (setf d (- q p)
                q (ash q -1)
                r p)))
      (setf p (ash p -1)))
    (nreverse network)))

(defmacro!-exported sortf (comparator &rest places)
  (if places
    `(tagbody
       ,@(mapcar
           #`(let ((,g!a #1=,(nth (car a1) places))
                   (,g!b #2=,(nth (cadr a1) places)))
               (if (,comparator ,g!b ,g!a)
                 (setf #1# ,g!b
                       #2# ,g!a)))
           (build-batcher-sn (length places))))))



;;;;;; NEW CODE FOR ANTIWEB

#+cl-ppcre
(defun-exported dollar-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 1)
       (string= (symbol-name s)
                "$"
                :start1 0
                :end1 1)
       (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))

(defun-exported prune-if-match-bodies-from-sub-lexical-scope (tree)
  (if (consp tree)
    (if (or (eq (car tree) 'if-match)
            (eq (car tree) 'when-match))
      (cddr tree)
      (cons (prune-if-match-bodies-from-sub-lexical-scope (car tree))
            (prune-if-match-bodies-from-sub-lexical-scope (cdr tree))))
    tree))

;; WARNING: Not %100 correct. Removes forms like (... if-match ...) from the
;; sub-lexical scope even though this isn't an invocation of the macro.
#+cl-ppcre
(defmacro!-exported if-match ((test str) conseq &optional altern)
  (let ((dollars (remove-duplicates
                   (remove-if-not #'dollar-symbol-p
                                  (flatten (prune-if-match-bodies-from-sub-lexical-scope conseq))))))
    (let ((top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>)) 0)))
      `(let ((,g!str ,str))
         (multiple-value-bind (,g!s ,g!e ,g!ms ,g!me) (,test ,g!str)
           (declare (ignorable ,g!e ,g!me))
           (if ,g!s
             (if (< (length ,g!ms) ,top)
               (error "ifmatch: too few matches")
               (let ,(mapcar #`(,(symb "$" a1) (subseq ,g!str (aref ,g!ms ,(1- a1))
                                                              (aref ,g!me ,(1- a1))))
                             (loop for i from 1 to top collect i))
                 ,conseq))
              ,altern))))))

(defmacro-exported when-match ((test str) conseq &rest more-conseq)
  `(if-match (,test ,str)
     (progn ,conseq ,@more-conseq)))
