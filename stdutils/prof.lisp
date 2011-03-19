;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prof.lisp
;;;; Purpose:       Profiling aids and other performance related indicators and counters
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

;; Simple profiling tools

(in-package :stdutils)

;;
;; Elapsed time counter
;;

(defun-exported make-timer ()
  (list (get-universal-time)))

(defun-exported reset-timer (timer)
  (setf (first timer) (get-universal-time)))

(defun-exported timer-elapsed (timer)
  (- (get-universal-time) (first timer)))

(defun-exported timer-print-elapsed (timer &optional (stream t))
  (let ((elapsed (timer-elapsed timer)))
    (format stream "Elapsed time: ~A:~A:~A"
	    (floor (/ elapsed 3600))
	    (floor (rem (/ elapsed 60) 60))
	    (rem elapsed 60))))

;; 
;; Make system tick counter
;; 	  

(defun-exported make-ticker (&optional (ticks-per-sec (ticker-ticks-per-second)))
  (assert (>= ticks-per-sec (ticker-ticks-per-second)))
  (list (get-internal-real-time) ticks-per-sec))

(defun-exported reset-ticker (ticker)
  (setf (first ticker) (get-internal-real-time)))

(defun-exported ticker-elapsed (ticker)
  (let ((elapsed (- (get-internal-real-time) (first ticker))))
    (/ elapsed (second ticker))))

(defun-exported ticker-ticks-per-second ()
  cl::internal-time-units-per-second)

(defun-exported ticker-print-elapsed (ticker &key (stream t))
  "Print time elapsed, in microseconds or another n parts per second time base"
  (let ((elapsed (ticker-elapsed ticker)))
    (format stream "Elapsed time (~A ticks per second): ~A" 
	    (second ticker)
	    elapsed)))

(defmacro-exported with-print-elapsed-time (( &key (base-units 1) (norm-f #'identity) (unit-name "units")) &rest body)
  "Does not handle bodies that return multiple values.  Returns
   seconds elapsed unless you set base-units to 1/N seconds"
  (with-gensyms (ticker value)
    `(let ((,ticker (make-ticker))
	   (,value ,@body))
       (format t "Elapsed time: ~A ~A~%" 
	       (funcall ,norm-f 
			(* (ticker-elapsed ,ticker)
			   ,base-units))
	       ,unit-name)
       ,value)))

(defmacro-exported with-print-clock-cycles ((cpu-ghz &key (norm-f #'identity) (unit-name "units")) &rest body)
  (with-gensyms (ticker value)
    `(let ((,ticker (make-ticker))
	   (,value ,@body))
       (format t "Elapsed time: ~A ~A~%" 
	       (funcall ,norm-f
			(* (ticker-elapsed ,ticker)
			   (* 1000000000 ,cpu-ghz)))
	       ,unit-name)
       ,value)))
		 
;;
;; Countdown timer by element count
;;

(defun-exported make-timer-remaining-by-count (total)
  (list 
   (get-universal-time) ;; start time
   total                ;; total items
   total                ;; remaining items
   (get-universal-time) ;; time left
   ))
     
(defun-exported timer-rbc-update (timer remaining)
  (let* ((duration (timer-elapsed timer))
	 (total (second timer))
	 (consumed (- total remaining))
	 (per-item (/ duration consumed))
	 (time-left (floor (* remaining per-item))))
    (setf (third timer) remaining)
    (setf (fourth timer) time-left)))

(defun-exported timer-rbc-items-remaining (timer)
  (third timer))

(defun-exported timer-rbc-time-remaining (timer)
  (fourth timer))

(defun-exported timer-rbc-print-time-remaining (timer)
  (let ((remain (timer-rbc-time-remaining timer)))
    (format t "Remaining Time: ~A:~A:~A~%"
	    (floor (/ remain 3600))
	    (floor (rem (/ remain 60) 60))
	    (rem remain 60))))

     

     
     
     
