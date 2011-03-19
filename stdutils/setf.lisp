;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          setf.lisp
;;;; Purpose:       Place and setf based functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

;; TAG LIST in a PLACE
;; - A collection of unique symbols maintained in a place
;; - Init with 'nil

(defmacro-exported set-tag (place tag)
  `(pushnew ,tag ,place))

(defmacro-exported clear-tag (place tag)
  `(setf ,place (delete ,tag ,place)))

(defmacro-exported has-tag (place tag)
  `(member ,tag ,place))

;; -----------------------------------------
;; dynamic version of defvar on a place

(defmacro-exported ensure-a-value (place value)
  "Ensures that place, if nil, has value"
  `(if (null ,place)
       (setf ,place ,value)
     ,place))

;; Anaphoric setf of an object slot for clos
(defmacro-exported asetf (accessor-exp new-val-expr)
  `(let ((it ,accessor-exp))
     (declare (ignorable it))
     (setf ,accessor-exp ,new-val-expr)))

;;(defmacro-exported asetf (obj slot-accessor exp)
;;  `(let ((it (,slot-accessor ,obj)))
;;     (declare (ignorable it))
;;     (setf (,slot-accessor ,obj) ,exp)))


;; Some standard place methods and building setf functions

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj) 
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access) 
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args) 
  (multiple-value-bind (vars forms var set access) 
                       (get-setf-expansion place) 
    (let ((g (gensym))) 
      `(let* ((,g ,test) 
              ,@(mapcar #'list vars forms) 
              (,(car var) (delete-if ,g ,access ,@args))) 
         ,set)))) 

(defmacro popn (n place) 
  (multiple-value-bind (vars forms var set access) 
                       (get-setf-expansion place) 
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
                ,set)))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list 
                              (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m) 
                                        (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m) 
                                        (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar 
                       #'(lambda (arg)
                           `(unless (,op ,(car rest) ,arg)
                              (rotatef ,(car rest) ,arg)))
                       (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

