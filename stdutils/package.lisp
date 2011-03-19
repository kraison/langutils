;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Our package definition; exports are defined in-line in the code
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :cl-user)

(defpackage :stdutils
  (:use :common-lisp :cl-ppcre)
  (:export
   ;; Standard utilities exports
   #:rec  ;; recursive function capture variable
   #:it   ;; anaphor variable
   #:left  ;; tree anaphor variable
   #:right ;; tree anaphor variable
   ))

(defpackage :stdutils.gds
  (:use :common-lisp :stdutils))

  
 