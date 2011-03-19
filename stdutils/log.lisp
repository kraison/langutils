;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          log.lisp
;;;; Purpose:       Simple logging interface
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004


(in-package :stdutils)

(defvar *log-all* nil)
(defvar *log-table* (hash)
  "Hash of symbols to (enabled stream|t close-on-stop)
   Enabled determines whether dynamic calls succeed.
   Stream is the stream to write to, t if console
   Close-on-Stop determines if the stream is closed on stop commands.")


(defmacro-exported deflog (name parents &key (expand t))
  "Define a log name and multiple possible
   parent logs that will result in writes if
   read is subscribed to the parent.  Parents do
   not need to be defined - this is a user accessed
   namespace."
    (let ((entry (gensym)))
      `(eval-when (eval-toplevel load-toplevel)
	 (let ((,entry ',(localize-symbol name :package :utils)))
	   (hash-put *log-heirarchy* ,entry 
		     ',(mapcar #'(lambda (x) (localize-symbol x :package :utils)) parents))
	   (unless ,expand
	     (suppress-log-expansion ,entry))))))

(defun-exported enable-log-expansion (name)
  "Use in *dot* notation to enable or disable the macroexpansion
   of write-log statements at compile time.  Used to avoid the
   performance penalty of checking subscriptions at runtime without
   having to comment out those lines: ie.

   (deflog test-log :expand nil)
   (format test-log \"This will not print.\")
   #.(enable-log-expansion)
   (format test-log \"This will print.\")
   #.(disable-log-expansion)
   (format test-log \"This will not print.\")"
  (allow-log-expansion (localize-symbol name :package :utils)))

(defun-exported disable-log-expansion (name)
  "See docs for sister function: langutils:enable-log-expansion"
  (suppress-log-expansion (localize-symbol name :package :utils)))

(defmacro-exported write-log (sym format-string &rest args)
  "Enter a log entry using the name of 'sym' as a key.
   Always returns nil; can throw a condition on error."
  (let ((write-sym (if (and (consp sym) 
			    (symbolp (car sym))
			    (equalp (symbol-name (car sym)) "QUOTE"))
		       (let ((entry (localize-symbol (second sym) :package :utils)))
			 `,entry)
		     (let ((entry (localize-symbol sym :package :utils)))
		       `',entry))))
    (if (log-expand-p (second write-sym))
	`(write-log-fn ,write-sym ,format-string ,@args)
      nil)))
    

;; ------------------------
;; Enable/disable logging
;; 
;; &key type - [:console | :file]
;; &key target - path, string filename or stream if :file
;;               else ignored

(defmacro-exported start-logging (&optional (sym :all) &key (file nil))
  "Start logging for 'sym' log entries, behavior is
   determined by type.  If sym is 't', then log all."
  ;; Enable logging
  (with-gensyms (fval symval)
    `(let ((,fval ,file)
	   (,symval ',sym))
       (when (not (symbolp ,symval))
	 (setf ,symval (second ,symval)))
       (when (eq ,symval :all) (setf *log-all* t))
       (hash-put *log-table* (localize-symbol ,symval :package :utils :exceptions '(:keyword))
		 (if ,fval
		     (typecase ,fval
		       (string (list t (open ,fval :direction :output :if-exists :supersede) t))
		       (pathname (list t (open ,fval :direction :output :if-exists :supersede) t))
		     (stream (list t ,fval nil)))
		     (list t t nil)))
       t)))

(defmacro-exported stop-logging (&optional (sym :all) &key (children t))
  "Erases log entry for sym"
  (with-gensyms (symval)
    `(let ((,symval ',sym))
      (when (eq ,symval :all) (setf *log-all* nil))
      (let ((entry (localize-symbol ,symval :package :utils)))
	(awhen (subscriber entry)
	  (when (log-close-on-stop it)
	    (close (log-stream it)))
	  (hash-rem *log-table* (localize-symbol ,symval :package :utils :exceptions '(:keyword))))
	(when ,children
	  (mapc #'(lambda (x) (stop-logging x)) (log-all-children entry))))
      t)))

(defun-exported toggle-logging (&optional (sym :all))
  "Pause/unpause logging for 'sym'"
  (if (eq sym :all)
    (setf *log-all* t)
    (awhen (hash-get *log-table* (localize-symbol sym :package :utils))
      (setf (log-enabled it) (not (log-enabled it)))))
  t)

;; --------------------------------------------------------------------------------------------------

;; -----------------------
;; Log statement expansion

(defvar *log-suppress-all* nil)
(defvar *log-suppressed-expansions* (hash))
  
(defun log-expand-p (name)
  (not (or *log-suppress-all*
	   (hash-get *log-suppressed-expansions* name))))

(defun suppress-log-expansion (name)
  (hash-put *log-suppressed-expansions* name t))

(defun allow-log-expansion (name)
  (hash-rem *log-suppressed-expansions* name))


;; -----------------------
;; Log subscriptions

(defun log-enabled (entry)
  (first entry))
(defun set-log-enabled (entry value)
  (setf (first entry) value))
(defsetf log-enabled set-log-enabled)
(defun log-stream (entry)
  (second entry))
(defun log-close-on-stop (entry)
  (third entry))

(defun subscription (sym)
  "Get subscription record for log named sym"
  (hash-get *log-table* sym))

;; -----------------------
;; Log Heirarchy

(defvar *log-heirarchy* (hash))

;; NOTE: No need to define a log if it doesn't have parents
;;       except to declare log names at the top of a file for
;;       readers of the code.
(defun subscriber (sym)
  "Depth first for parent name for which
   there is a subsciption"
  (let ((entry (localize-symbol sym :package :utils)))
    (acond ((subscription entry) ;; If this is subscribed, return record
	    it)
	   ((hash-get *log-heirarchy* entry) ;; Else if has parents, find subscriber in parents
	    (some #'subscriber it))
	   (t nil)))) ;; failed, no subscribers

(defun log-all-children (sym)
  "Utility func for stop-logging; can start individual logs but
   then clear them all by calling stop on an unsubscribed parent"
  (let ((entry (localize-symbol sym :package :utils))
	(keys (hash-keys *log-heirarchy*)))
    (flatten (collect #'(lambda (x) (when (find entry (hash-get *log-heirarchy* x))
				      (cons x (log-all-children x))))
		      keys))))

;; ---------------------------
;; Function to write the log

(defun write-log-fn (sym format &rest rest)
  (let ((entry (if *log-all* :all sym)))
    (awhen (subscriber entry)
	   (awhen (and (log-enabled it) (log-stream it))
		  (cond ((eq it t)
			 (apply #'format t format rest)
			 (format t "~%")
			 (force-output t))
			((streamp it)
			 (apply #'format it format rest)
			 (format it "~%")
			 (force-output it))
			(t (error "Bad stream type ~A for log ~A with message ~A." it sym (apply #'format nil format rest))))))))

  