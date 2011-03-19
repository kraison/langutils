;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          iterator.lisp
;;;; Purpose:       Simple iterator object for gds
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2005
;;;;

(in-package :stdutils.gds)

(defprotocol iterator
  (next next-p drop-last reset clear))
