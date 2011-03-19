;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          graphviz.lisp
;;;; Purpose:       A framework for generating dot files and calling graphviz
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  January 2006

(in-package :utils.gds)

;; -------------------
;; Attributes
;; -------------------

(defclass attributed-mixin ()
  ((attribute-table :accessor attribute-table :initarg :attributes :initform nil)
   (attribute-key-test :accessor attribute-key-test :initarg :attributes-test :initform #'eq)))

(defmethod get-attribute ((obj attributed-mixin) (name symbol))
  (get-value (attribute-table obj) name))

(defmethod (setf get-attribute) (value (obj attributed-mixin) (name symbol))
  (setf (get-value (attribute-table obj) name) value))

(defvar *attributed-initargs* nil
  "Enable automatic parsing of registered initargs during object creation")

(defun add-registered-attribute (class attribute)
  (aif (assoc-get *attributed-initargs* class)
       (pushnew attribute (cdr it))
       (assoc-setf *attributed-initargs* class (list attribute))))

(defun coercible (object class)
  (handler-case 
      (when (coerce object class) t)
    (error () nil)))

(defmethod initialize-instance :after ((obj attributed-mixin) &rest initargs &key &allow-other-keys)
  "Ensure attribute table and provide support for auto setting of 
   registered initargs"
  (unless (attribute-table obj)
    (setf (attribute-table obj)
	  (make-instance 'assoc-table :key-test (attribute-key-test obj))))
  (loop for class-rec in *attributed-initargs* do
       (when (or (eq (car class-rec) (type-of obj))
		 (coercible obj (car class-rec)))
	 (loop for initarg in (cdr class-rec) do
	      (awhen (position initarg initargs)
		(setf (get-attribute obj initarg) (nth (1+ it) initargs)))))))

(eval-when (:compile-toplevel :load-toplevel :compile)
  (defmacro def-attribute-accessor (name label-name &optional (class 'attributed-mixin))
    (with-gensyms (obj value)
      `(progn
	 ;; Enforce a standard of keyword labels
	 (assert (eq (symbol-package ,label-name) (find-package :keyword)))
	 (add-registered-attribute ',class ,label-name)
	 (defmethod ,name ((,obj ,class))
	   (get-attribute ,obj ,label-name))
	 (defmethod (setf ,name) (,value (,obj ,class))
	   (setf (get-attribute ,obj ,label-name) ,value))))))

;; ----------------
;; ID's
;; ----------------

(defvar *unique-id-counter* 0)

(defclass simple-id-mixin ()
  ((id :accessor object-id :initarg :id :initform (incf *unique-id-counter*))))

(defmethod initialize-instance :after ((object simple-id-mixin) &rest rest)
  (declare (ignore rest))
  (assert (object-id object)))

;; ------------------
;; Nodes and edges
;; ------------------

(defclass graphviz-edge (attributed-mixin simple-id-mixin)
  ((type :accessor edge-type :initarg :type :initform '--)
   (lhs :accessor edge-lhs :initarg :lhs :initform nil)
   (lhs-port :accessor edge-lhs-port :initarg :lhs-port :initform nil)
   (rhs :accessor edge-rhs :initarg :rhs :initform nil)
   (rhs-port :accessor edge-rhs-port :initarg :rhs-port :initform nil)))

(defmethod print-object ((obj graphviz-edge) stream)
  (let ((srclabel (aif-ret (node-label (edge-lhs obj))
			   (object-id (edge-lhs obj))))
	(dstlabel (aif-ret (node-label (edge-rhs obj))
			   (object-id (edge-rhs obj)))))
    (format stream "#EDGE[~A -> ~A]" srclabel dstlabel)))

(def-attribute-accessor edge-label :label graphviz-edge)

(defclass graphviz-node (attributed-mixin simple-id-mixin)
  ((edges :accessor node-edges :initarg :edges :initform (make-instance 'assoc-table))))

(def-attribute-accessor node-label :label graphviz-node)

(defmethod get-edge ((node graphviz-node) id)
  (get-value (node-edges node) id))

(defmethod add-edge ((node graphviz-node) (edge graphviz-edge) &optional rest)
  (declare (ignore rest))
  (add (node-edges node) (object-id edge) edge))

(defmethod remove-edge ((node graphviz-node) (edge graphviz-edge))
  (drop (node-edges node) (object-id edge)))

;; -----------------
;; Graph
;; -----------------

(defclass graphviz-graph (attributed-mixin simple-id-mixin)
  ((type :accessor graph-type :initarg :type :initform :digraph) ;; graph, digraph, subgraph
   (strict :accessor graph-strict-p :initarg :strict :initform nil)
   (children :accessor graph-children :initarg :nodes :initform (make-instance 'hashed-table))
   (edges :accessor graph-edges :initarg :edges :initform (make-instance 'hashed-table))))

;; EDGES

(defmethod get-edge ((graph graphviz-graph) id)
  (get-value (graph-edges graph) id))

(defmethod add-edge ((graph graphviz-graph) (edge graphviz-edge) &optional rest)
  (declare (ignore rest))
  (add (graph-edges graph) (object-id edge) edge)
  edge)

(defmethod add-edge ((graph graphviz-graph) id &optional edge)
  (setf (object-id edge) id)
  (add (graph-edges graph) id edge))

(defmethod remove-edge ((graph graphviz-graph) (edge graphviz-edge))
  (drop (graph-edges graph) (object-id edge))
  edge)

(defmethod remove-edge ((graph graphviz-graph) id)
  (drop (graph-edges graph) id))

;; CHILDREN

(defmethod get-child ((graph graphviz-graph) id)
  (get-value (graph-children graph) id))

(defmethod add-child ((graph graphviz-graph) (child simple-id-mixin) &optional rest)
  (declare (ignore rest))
  (add (graph-children graph) (object-id child) child)
  child)

(defmethod add-child ((graph graphviz-graph) id &optional child)
  (setf (object-id child) id)
  (add (graph-children graph) id child))

(defmethod remove-child ((graph graphviz-graph) node)
  (setf (graph-children graph) (remove node (graph-children graph)))
  node)

;; ------------------------
;; Simple graph operations
;; ------------------------

(defmethod walk-graph (fn (graph graphviz-graph) &rest options)
  (let ((visited (make-instance 'hashed-table)))
    (declare (special visited))
    (%walk-graph fn graph)))

(defmethod %walk-graph (fn (graph graphviz-graph))
  (declare (special visited))
  (map-values (curry #'conditionally-visit-node fn)
	      (graph-children graph))
  (funcall fn graph))

(defun conditionally-visit-node (fn node)
  (declare (special visited))
  (unless (get-value visited (object-id node))
    (%walk-graph fn node)))

(defmethod %walk-graph (fn (node graphviz-node))
  (declare (special visited))
  (funcall fn node)
  (add visited (object-id node) node)
  (map-values (lambda (edge)
		(let ((rhs-node (edge-rhs edge)))
		  (unless (get-value visited (object-id rhs-node))
		    (%walk-graph fn rhs-node))))
	      (node-edges node)))

(defmethod all-edges ((graph graphviz-graph))
  (let ((edges nil))
    (walk-graph (lambda (element)
		 (typecase element
		   (graphviz-graph (setf edges (append edges (table-values (graph-edges element)))))
		   (graphviz-node (setf edges (append edges (table-values (node-edges element)))))))
	       graph)
    edges))

(defmethod all-nodes ((graph graphviz-graph))
  (let ((nodes nil))
    (walk-graph (lambda (element)
		  (typecase element
		    (graphviz-node (push element nodes))))
		graph)
    nodes))

;; --------------------
;; DOT Format Renderer
;; --------------------

(defmethod render :before ((graph graphviz-graph) stream)
  "Lots of error checking"
  (assert stream)
  (assert (member (graph-type graph) '(:graph :digraph :subgraph)))
  (assert (not (and (graph-strict-p graph) (eq (graph-type graph) ':subgraph))))
  (unless (graph-children graph)
    (error "Graph ~A is empty" (object-id graph))))

(defmethod render-attributes ((obj attributed-mixin) separator stream)
  (flet ((cvtsym (sym)
	   (string-downcase
	    (symbol-name sym))))
    (format stream  
	    (format nil "~~{~~A = ~~A~~^~A~~}" separator)
	    (shuffle 
	     (mapcar #'cvtsym (table-keys (attribute-table obj)))
	     (table-values (attribute-table obj))))))

(defmethod render ((graph graphviz-graph) stream)
  (awhen (graph-strict-p graph) (princ it stream))
  (princ #\Space stream)
  (princ (graph-type graph) stream)
  (princ #\Space stream)
  (awhen (object-id graph) (princ it stream))
  (princ #\Space stream)
  (princ #\{ stream)
  (princ #\Newline stream)
  (unless (empty (attribute-table graph))
    (render-attributes graph
		       (format nil ";~%")
		       stream)
    (format stream ";~%"))
  (map-values (lambda (obj) 
		(render obj stream)
		(princ #\; stream)
		(princ #\Newline stream))
	      (graph-children graph))
  (mapc (lambda (edge)
	  (render edge stream)
	  (princ #\; stream)
	  (princ #\Newline stream))
	(all-edges graph))
  (princ #\Newline stream)
  (princ #\} stream)
  (princ #\Newline stream))

(defmethod render ((node graphviz-node) stream)
  (princ (object-id node) stream)
  (princ #\Space stream)
  (unless (empty (attribute-table node))
    (format stream " [ ")
    (render-attributes node ", " stream)
    (format stream " ]")))
  

(defmethod render ((edge graphviz-edge) stream)
  (princ (object-id (edge-lhs edge)) stream)
  (princ #\Space stream)
  (princ (edge-type edge) stream)
  (princ #\Space stream)
  (princ (object-id (edge-rhs edge)) stream)
  (unless (empty (attribute-table edge))
    (format stream " [ ")
    (render-attributes (attribute-table edge) ", " stream)
    (format stream " ]")))

;; ----------------
;; Call external
;; ----------------

(defparameter *graphviz-root-directory* "/Applications/Graphviz.app/Contents/MacOS/")

(defun graphviz-converter-pgm (type)
  (merge-pathnames 
   type
   *graphviz-root-directory*))

(defun render-to-file (graph filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (render graph out)))

(defun generate-graphviz-file (graph root-name &key (renderer :dot) 
			                            (file-type :jpg) 
			                            (dir (port:default-directory)))
  (let ((ftype (string-downcase (symbol-name file-type)))
	(rtype (string-downcase (symbol-name renderer))))
    (let ((stream (port:pipe-output 
		   (format nil "~A -T~A -o ~A/~A.~A"
			   (graphviz-converter-pgm rtype)
			   ftype dir root-name ftype))))
      (unwind-protect 
	   (render graph stream)
	(close stream)))
    (merge-pathnames 
     (format nil "~A.~A" root-name ftype)
     dir)))

(defmethod view-graphviz-graph ((graph graphviz-graph) &key (renderer :dot) (file-type :jpg))
  (let ((file (generate-graphviz-file graph "tmp" :renderer renderer :file-type file-type)))
    (port:run-prog "/Applications/Firefox.app/Contents/MacOS/Firefox" :args (list (namestring file)))))

;;(defun annotate-graph 

;; ---------------
;; Sample graphs
;; ---------------

(defun simple-graph1 ()
  (let ((graph (make-instance 'graphviz-graph))
        (node1 (make-instance 'graphviz-node :label "one"))
	(node2 (make-instance 'graphviz-node :label "two"))
	(node3 (make-instance 'graphviz-node :label "three")))
    (add-edge node1 (make-instance 'graphviz-edge :type '-> :label "A" :lhs node1 :rhs node2))
    (add-edge node1 (make-instance 'graphviz-edge :type '-> :label "B" :lhs node1 :rhs node3))
    (add-child graph node1)
    (add-child graph node2)
    (add-child graph node3)
    graph))


;; -------------------
;; DOT Parser
;; -------------------

;; Write a YACC style parser/generator that
;; operates over attributed parse tree instances
;; that translate to/from a domain graph/tree/DS/record

;; How to make a grammar easily invertable?
  