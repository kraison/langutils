;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:meta-system (:use #:asdf #:cl))
(in-package #:meta-system)

(defsystem meta
  :components ((:file "package")
	       (:file "meta-src" :depends-on ("package"))))

