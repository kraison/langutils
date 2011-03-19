;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file.lisp
;;;; Purpose:       File related utility functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :stdutils)

;;
;; Operations over files
;;

(defun split-file (infile outfile1 outfile2 &key (type :half))
  "Allows you to split a file by :alternate lines or to split
   it in half by length"
  (with-open-file (in infile :direction :input)
    (with-open-file (out1 outfile1 :direction :output :if-exists :supersede)
      (with-open-file (out2 outfile2 :direction :output :if-exists :supersede)
	(let ((split-size (/ (file-length in) 2))
	      (sofar 0))
	  (do-count-contentful-lines (line count in)
	     (case type
	       (:half 
		(incf sofar (length line))
		(if (> sofar split-size)
		    (format out2 "~A~%" line)
		  (format out1 "~A~%" line)))
	       (:alternate
		(if (= 0 (mod count 2))
		    (format out2 "~A~%" line)
		  (format out1 "~A~%" line))))))))))

;;
;; Slurp
;;

(defun-exported slurp-file (filename)
  "Read a file's contents to a string"
  (with-open-file (stream filename :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

;;
;; With and do macros on files as streams
;; 

(let ((g (gensym)))
  (defun-exported read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro-exported do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

;; Walk through all lines from a stream until eof, 
;; returning the last line read
(defmacro-exported do-stream-lines ((label stream) &rest body)
  `(do ((,label (read-line ,stream nil 'eof) (read-line ,stream nil 'eof)))
       ((eq ,label 'eof) nil)
     ,@body))

(defmacro-exported do-count-contentful-lines ((line-var count-var stream) &rest body)
  `(let ((,count-var 0))
     (do-stream-lines (,line-var ,stream)
       (unless (string= ,line-var "")
	 ,@body
	 (incf ,count-var)))))

(defmacro-exported do-contentful-lines ((label stream &key (count nil)) &rest body)
  (let* ((cvar (gensym))
	 (init `(let ((,cvar ,count))))
         (iterator `(do-stream-lines (,label ,stream)))
	 (inc-n-test `((decf ,cvar) (when (<= ,cvar 0) (return))))
	 (new-body `((unless (string= ,label "") ,@body))))
    (if count
	(append init (list (append iterator inc-n-test new-body)))
      (append iterator new-body))))

;;
;; CSV file extensions to cllib functions
;;

;;(defun-exported csv-read-lines (file &aux vectors)
;;  "Returns a list of vectors from the input file"
;;  (with-open-file (s file :direction :input)
;;    (while (not (eof-p s))
;;      (push (csv-parse-string (read-line s)) vectors)))
;;  (nreverse vectors))

;;
;; File system heirarchy operations
;;

(defun probe-directory (filename)
  "Check whether the file name names an existing directory."
  ;; based on
  ;; From: Bill Schelter <wfs@fireant.ma.utexas.edu>
  ;; Date: Wed, 5 May 1999 11:51:19 -0500
  ;; fold the name.type into directory
  (flet ((un-unspecific (value)
           (if (eq value :unspecific) nil value)))
    (let* ((path (pathname filename))
	   (name (un-unspecific (pathname-name path)))
	   (type (un-unspecific (pathname-type path)))
	   (new-dir
	    (cond ((and name type) (list (concatenate 'string name "." type)))
		  (name (list name))
		  (type (list type))
		  (t nil))))
      (when new-dir
	(setq path (make-pathname
		    :directory (append (un-unspecific (pathname-directory path))
				       new-dir)
		    :name nil :type nil :version nil :defaults path)))
      #+allegro (excl::probe-directory path)
      #+clisp (values
	       (ignore-errors
		 (#+lisp=cl ext:probe-directory #-lisp=cl lisp:probe-directory
			    path)))
      #+cmu (eq :directory (unix:unix-file-kind (namestring path)))
      #+lispworks (lw:file-directory-p path)
      #+sbcl (eq :directory (sb-unix:unix-file-kind (namestring path)))
      #-(or allegro clisp cmu lispworks sbcl)
      (probe-file path))))

(defun-exported walk-directories (rootdir &rest args &key filef dirf hidden 
					  (on-entry t) (ignore-dotfiles t)
					  ignore-dirs)
  (declare (ignore hidden))
  (when (probe-directory rootdir)
    (when (and on-entry dirf)
      (funcall dirf rootdir))
    (mapc #'(lambda (path) 
	      (let ((dirname (make-pathname 
			      :host (pathname-host path)
			      :directory (append (pathname-directory path)
						 (mklist (pathname-name path)))
			      :device (pathname-device path)
			      :version (pathname-version path))))
		(if (and (not (and ignore-dotfiles
				   (pathname-name path)
				   (eq #\. (char (pathname-name path) 0))))
			 (probe-directory dirname)
			 (not (and ignore-dirs
				   (member (last1 (pathname-directory path) )
					   ignore-dirs :test #'equal))))
		    (apply 'walk-directories dirname args)
		    (when filef (funcall filef path)))))
	  (directory (merge-pathnames rootdir "*.*")))
    (when (and (not on-entry) dirf) (funcall dirf rootdir))))

(defun-exported first-nonwhite-char (string)
  "Find the first non-whitespace character on this line"
  (awhen (cl-ppcre:scan-to-strings "\\S" string) 
	 (char it 0)))

(defun-exported count-lisp-lines (filename &key ignore-comments ignore-strings)
  (let ((count 0))
    (with-open-file (file filename :direction :input)
      (do-stream-lines (line file)
        (unless (awhen (first-nonwhite-char line)
		       (or (and ignore-comments (eq it #\;))
			   (and ignore-strings (eq it #\"))))
	  (incf count))))
    count))

(defun-exported count-lisp-files (directory &key (extension "lisp") ignore-comments ignore-strings 
					    print-files print-dirs (ignore-dirs '("_darcs" ".svn" ".cvs" ".hg")))
  "Line count for all lisp files under provided directory"
  (declare (ignore print-dirs))
  (let ((total 0)
	(clean-total 0)
	(files 0)
	(dir-total 0))
    (mapc (lambda (dir)
	    (walk-directories dir
		      :filef #'(lambda (path)
				 (when (equal (pathname-type path) extension)
				   (let ((file-count (count-lisp-lines 
						      path 
						      :ignore-comments ignore-comments
						      :ignore-strings ignore-strings)))
				     (incf files)
				     (incf dir-total file-count)
				     (incf clean-total file-count)
				     (when print-files (format t "~A: ~A~%" 
							       (namestring path) 
							       file-count)))))
;; 		      :dirf #'(lambda (path)
;; 				(incf total dir-total)
;; 				(when print-dirs
;; 				  (format t "Directory: ~A~%Total: ~A~%" 
;; 					  (namestring path) dir-total))
;; 				(setf dir-total 0))
		      :on-entry nil
		      :ignore-dirs ignore-dirs
		      :ignore-dotfiles t))
	  (mklist directory))
    (format t "Total lines: ~A~%" total)
    (values clean-total total files)))
							
(defun-exported directory-pathname (file)
  "Return the path of the directory of the file."
  ;; This function has only been tested in alegro and mcl
  #+allegro (excl:path-pathname file)
  #+(or mcl ccl) (pathname-directory file)
  #-(or mcl ccl allegro) nil)
;;  #-(or mcl allegro) (excl:path-pathname file) ;; Should fail, but is easy to extend :)
