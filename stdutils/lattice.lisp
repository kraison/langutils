;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lattice.lisp
;;;; Purpose:       A framework for generating dot files and calling graphviz
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  January 2006
;;;;
;;;; NOTES:
;;;;  Generate lattices from a set of partial orders

(in-package :utils.gds)

;;
;; Partial orders
;;

(defparameter *sample-exclusive-state-sets*
  '((e f tt)
    (s r)))

(defparameter *sample-partial-orders*
  '((S R)
    (E TT)
    (F TT)))

(defvar *partial-orders* nil)

(defun factor-partial-orders (orders)
  (let ((pairs nil))
    (mapc (lambda (order-list)
	    (mapl (lambda (list)
		    (when (and (not (null list))
			       (not (null (cdr list)))
			       (not (null (cadr list))))
		      (push (list (car list) (cadr list)) pairs)))
		  order-list))
	  orders)
    pairs))

(defun dominates (f1 f2)
  (declare (special *partial-orders*))
  (loop for pair in *partial-orders* do
       (when (and (eq f1 (first pair))
		  (eq f2 (second pair)))
	 (return-from dominates t)))
  nil)

(defmethod dominates-node ((n1 list) (n2 list))
  (let ((pos 0)
	(neg 0))
    (loop for f1 in n1 do
	 (loop for f2 in n2 do
	      (when (dominates f1 f2)
		(incf pos))
	      (when (dominates f2 f1)
		(incf neg))))
    (values (> pos neg) (- pos neg))))

(defun lattice-label-from-features (features)
  (format nil "\"~{~A~^ ~}\"" features))

;;
;; GraphViz lattice rep
;;

(defclass lattice-node (graphviz-node)
  ((features :accessor lattice-node-features :initarg :features)))

(defmethod dominates-node ((n1 lattice-node) (n2 lattice-node))
  (dominates-node (lattice-node-features n1) 
		  (lattice-node-features n2)))

(defun make-lattice-node (features)
  (make-instance 'lattice-node 
		 :label (lattice-label-from-features features)
		 :features features))

(defmethod make-lattice-edge ((head graphviz-node) (tail graphviz-node))
  (let ((edge (make-instance 'graphviz-edge :type '-> :lhs head :rhs tail)))
    (add-edge head edge)
    edge))
	    
;;
;; GraphViz lattice construction
;;

(defun subordinate-by-one? (node1 node2)
  (eq (mvretn 2 (dominates-node node1 node2)) 1))

(defun one-difference (l1 l2)
  (= (length l1) 
     (1+ (length (intersection l1 l2)))))

(defun requires-link? (node1 node2)
  "Looks for minimal changes - works for fully articulated 
   lattices."
  (and (subordinate-by-one? node1 node2)
       (one-difference (lattice-node-features node1) 
		       (lattice-node-features node2))))

(defun find-minimal-subordinate-nodes (highest rest dominates)
  "Finds subordinate nodes based on requires-link? which is the
   minimal possible dominance."
  (cond ((null rest)
	 dominates)
	((requires-link? highest (car rest))
	 (find-minimal-subordinate-nodes highest (cdr rest) (cons (car rest) dominates)))
	(t (find-minimal-subordinate-nodes highest (cdr rest) dominates))))

(defun find-all-subordinate-nodes (highest rest dominates)
  "Find all dominance relations that highest has in rest"
  (cond ((null rest)
	 dominates)
	((dominates-node highest (car rest))
	 (find-all-subordinate-nodes highest (cdr rest) (cons (car rest) dominates)))
	(t (find-all-subordinate-nodes highest (cdr rest) dominates))))

(defun make-dominance-edges (list)
  "Add edges assumes list is sorted in dominance order"
  (when (>= (length list) 2)
    (let ((dom (find-minimal-subordinate-nodes (car list) (cdr list) nil)))
      (mapc (curry #'make-lattice-edge (car list)) dom))))

(defun max-path-length (start end)
  "Finds the longest path in the graph between start and end nodes"
  (let ((max 0))
    (loop for edge in (table-values (node-edges start)) do
	 (cond ((eq (edge-rhs edge) end)
		(setf max (max max 1)))
	       (t (setf max (max max
				 (1+ (max-path-length (edge-rhs edge) end)))))))
    max))

(defun prune-non-minimal-paths (list)
  "Remove any edges dominated in the list order"
  (loop for edge in (table-values (node-edges (car list))) do
       (when (> (max-path-length (edge-lhs edge) (edge-rhs edge)) 1)
	 (remove-edge (car list) edge))))

(defun exclusive-states-to-nodes (states)
  (mapcar #'make-lattice-node 
	  (apply #'combinations states)))

(defun make-graphviz-lattice (exclusive-states &key partial-orders)
  (declare (special *partial-orders*))
  (let ((*partial-orders* (aif-ret partial-orders *partial-orders*))
	(gnodes (sort (exclusive-states-to-nodes exclusive-states)
		      #'dominates-node))
	(lattice (make-instance 'graphviz-graph)))
    (declare (special *partial-orders*))
    (mapl #'make-dominance-edges gnodes)
    (mapl #'prune-non-minimal-paths gnodes)
    (mapc (curry #'add-child lattice) gnodes)
    lattice))

(defun features-to-nodes (features)
  (mapcar #'make-lattice-node features))

(defun make-partial-lattice (node-features &key partial-orders)
  (declare (special *partial-orders*))
  (let ((*partial-orders* (aif-ret partial-orders *partial-orders*))
	(gnodes (sort (features-to-nodes node-features) #'dominates-node))
	(lattice (make-instance 'graphviz-graph)))
    (declare (special *partial-orders*))
    (mapl #'make-edges gnodes)
    (mapc (curry #'add-child lattice) gnodes)
    lattice))


;; ===================
;; Sample lattices
;; ==================

(defparameter *label-preferences1*
  '((E !E)   ;; empty, contains
    (XYA XA) ;; aligned xy, aligned x, aligned y, unaligned
    (XYA YA)
    (XA !A)
    (YA !A)
    (SQ RTR)))

(defparameter *label-preferences2*
  '((E !E)   ;; empty, contains
    (XYA XA) ;; aligned xy, aligned x, aligned y, unaligned
    (XYA YA)
    (XA !A)
    (YA !A)
    (SQ RTR)
    (LC !LC)))

(defparameter *features1*
  '((SQ RTR)
    (E !E)
    (XYA XA YA !A)))

(defparameter *features2*
  '((SQ RTR)
    (E !E)
    (XYA XA YA !A)
    (LC !LC)))

(defparameter *active-features1*
  '((SQ E !A)
    (SQ !E XYA)
    (RTR XYA E)))

(defparameter *active-features2*
  '((SQ E !A LC)
    (SQ !E XYA !LC)
    (RTR XYA E !LC)))

(defun annotate-node (graph node-features attr-name attr-value)
  (loop for node in (all-nodes graph) do
       (when (every (lambda (feature)
		      (member feature node-features))
		    (lattice-node-features node))
	 (setf (get-attribute node attr-name) attr-value))))

(defun mark-active-nodes (graph list attr-name attr-value)
  (loop for features in list do
       (annotate-node graph features attr-name attr-value)))

(defun make-label-diagram-1 (name)
  (let ((graph (make-graphviz-lattice *features1* :partial-orders *label-preferences1*)))
    ;; label nodes of interest?
    (mark-active-nodes graph *active-features* :color "blue")
    (generate-graphviz-file graph name :dir "~/Desktop" )))

(defun make-label-diagram-2 (name)
  (let ((graph (make-graphviz-lattice *features2* :partial-orders *label-preferences2*)))
    ;; label nodes of interest?
    (mark-active-nodes graph *active-features* :color "blue")
    (generate-graphviz-file graph name :dir "~/Desktop" )))
