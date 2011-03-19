;; 
;; Basic Macros from Paul Graham, etc
;;

(in-package :stdutils)

(defmacro-exported nullit (expr)
  "Good for interaction at the repl; return null to the invoking expression 
   regardless of the return value of expr"
  `(progn ,expr nil))

(defmacro-exported mac1 (expr) 
  "Abbreviation for macroexpand-1"
  `(pprint (macroexpand-1 ',expr)))

(defmacro-exported when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro-exported dis1 (args &body body)
  `(disassemble
    (compile nil
      (lambda ,args
	,@body))))

(defmacro-exported when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defun-exported pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defmacro-exported defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all   `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun-exported anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
    call))

(defun-exported anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun-exported anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defun-exported dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                     binds)
        ,(dbind-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))

(defmacro-exported as-functions (names &body body)
  (let ((syms (gensyms (length names)))
	(args (gensym)))
    `(let ,(gather (list syms names))
       (macrolet 
	   ,(mapcar (lambda (sym name)
		      `(,name (&rest ,args) 
			      `(funcall ,',sym ,@,args)))
		    syms names)
	 ,@body))))



;;
;; Creating local environments
;; 

(defmacro-exported with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan 
                 #'(lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar #'(lambda (p)
                                 `(,p (aref ,gar 
                                            ,row 
                                            ,(incf col))))
                              pat))
                 pats))
         ,@body))))

(defmacro-exported with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

(defmacro-exported with-struct ((name . fields) struct &body body)
  (declare (ignore name))
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (slot-value ,gs ',f)))
                     fields)
         ,@body))))

(defmacro-exported with-struct-slots ((name . fields) struct &body body)
  (declare (ignore name))
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (symbol-macrolet
	   ,(mapcar #'(lambda (f)
			`(,f (slot-value ,gs ',f)))
		    fields)
         ,@body))))

;;(defmacro with-places (pat seq &body body)
;;  (let ((gseq (gensym)))
;;    `(let ((,gseq ,seq))
;;       ,(wplac-ex (destructuring-bind pat gseq #'atom) body))))

(defun-exported wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                                 binds)
        ,(wplac-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))

(defmacro-exported propmacro (propname)
  "Create a macro that gets a specific property
   from the property list of a symbol"
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro-exported propmacros (&rest props)
  "Create multiple property get macros"
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props)))

(defun-exported parallel-expand (keyword body)
  (cond 
   ((listp body)
    (if (eql (car body) keyword)
	(values (cdr body) (length (cdr body)))
      (let* ((exp-count nil)
	     (expansions
	      (loop for elem in body collecting
		(multiple-value-bind (exp num) (parallel-expand keyword elem)
		  (when num
		    (if (and exp-count (not (eql exp-count num)))
			(error "Inconsistent expansion counts")
		      (setq exp-count num)))
		  (list num exp)))))
	(if exp-count
	    (values (loop for counter from 0 upto (1- exp-count) collecting
		      (mapcar (lambda (x) (if (car x) (elt (cadr x) counter) (cadr x))) expansions))
		    exp-count)
	  (values (mapcar #'cadr expansions) nil)))))
   (t (values body nil))))
	
      
(defmacro-exported paralet (assignments &body body)
  `(let (,@(mapcan (lambda (x) (parallel-expand :each x)) assignments))
     ,@body))

(defmacro-exported def-simple-accessor (name args body)
  (let ((set-name (intern (concatenate 'string "SET-" (symbol-name name)))))
    `(progn
       (defun ,name ,args
	 (progn ,body))
       (defun ,set-name ,(append args (list 'value))
	 (setf ,body value))
       (defsetf ,name ,set-name))))
