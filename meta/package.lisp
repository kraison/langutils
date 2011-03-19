;;;; See meta.lisp for copyright-information

(in-package :cl-user)

(defpackage :meta
  (:use #:common-lisp)
  (:export #:with-string-meta
	   #:with-list-meta
	   #:with-stream-meta
	   #:enable-meta-syntax
	   #:disable-meta-syntax
	   #:index
	   #:end
	   #:meta-match))
