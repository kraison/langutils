(in-package :utils)

;;
;; A simple doubly-linked list implementation (in progress)
;;

(defun dcons (prev first rest)
  "A double cons, compatible with cdr"
  (cons (cons first prev) rest))

;; need to define setf
(defun dcar (dcons) (caar dcons))
(defun dcdr (dcons) (cdr dcons))
(defun dprv (dcons) (cdar dcons))
(defun dfirst (dcons) (caar dcons))
(defun dsecond (dcons) (car (second dcons)))
(defun dlist (&rest elts)
  (if (null elts) nil
      (let ((this (dcons nil (first elts) nil))
	    (next (dlist (cdr elts))))
	(setf (dcdr this) next)
	(setf (dprv next) this)
	this)))
