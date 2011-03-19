;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          allegro
;;;; Purpose:       Various allegro-specific utilities
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  November 2004
;;;;

(in-package :stdutils)


(defmacro-exported with-static-memory-allocation-old ((&key (free-bytes-new-pages 2000000) (free-bytes-new-other 1000000) 
							(free-percent-new 70)) &rest body)

  #+allegro `(let ((old-fp (sys:gsgc-parameter :free-bytes-new-pages))
		   (old-fo (sys:gsgc-parameter :free-bytes-new-other))
		   (old-pn (sys:gsgc-parameter :free-percent-new)))
	       (setf (sys:gsgc-parameter :free-bytes-new-pages) ,free-bytes-new-pages)
	       (setf (sys:gsgc-parameter :free-bytes-new-other) ,free-bytes-new-other)
	       (setf (sys:gsgc-parameter :free-percent-new) ,free-percent-new)
	       (sys::tenuring
		,@body)
	       (setf (sys:gsgc-parameter :free-bytes-new-pages) old-fp)
	       (setf (sys:gsgc-parameter :free-bytes-new-other) old-fo)
	       (setf (sys:gsgc-parameter :free-percent-new) old-pn))
  #-allegro `(progn ,@body))

(defmacro-exported with-huge-memory-requirements (&body body)
  "Makes sure the system is ready for huge memory demands by adjusting page sizes and such."
  #+allegro `(let ((old-fp (sys:gsgc-parameter :free-bytes-new-pages))
		   (old-fo (sys:gsgc-parameter :free-bytes-new-other))
		   (old-pn (sys:gsgc-parameter :free-percent-new)))
	       (setf (sys:gsgc-parameter :free-bytes-new-pages) 10000000)
	       (setf (sys:gsgc-parameter :free-bytes-new-other) 10000000)
	       (setf (sys:gsgc-parameter :free-percent-new) 50)
	       (sys::tenuring ,@body)
	       (setf (sys:gsgc-parameter :free-bytes-new-pages) old-fp)
	       (setf (sys:gsgc-parameter :free-bytes-new-other) old-fo)
	       (setf (sys:gsgc-parameter :free-percent-new) old-pn))
  #-allegro `(progn ,@body)
  )

(defmacro-exported with-static-memory-allocation (() &rest body)
  `(progn ,@body))
