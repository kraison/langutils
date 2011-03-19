;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; ASD File for Think Utilities Module

(defpackage #:utils.system
  (:use #:cl #:asdf))

(in-package #:utils.system)

;; Kevin Rosenberg mop hacks
;;#+(or allegro cmu lispworks sbcl scl openmcl)
;;(pushnew :kmr-mop cl:*features*)

(defsystem #:utils
    :description "Think Utilities: A set of helpful utilities used by the Think system"
    :version "1.0"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "Public Domain"    
    :components ((:file "package")
		 (:file "system" :depends-on ("package"))         ;; global system macros
		 (:file "imports" :depends-on ("system"))         ;; import and re-export :cllib and :kmrcl
		 (:file "lists" :depends-on ("system"))           ;; list related utilities, tree walking, searching, etc
		 (:file "macros" :depends-on ("system"))          ;; Useful macros, such as aif, awhen, etc.
		 (:file "iteration" :depends-on ("system"))       ;; iteration related utilities
		 (:file "conditionals" :depends-on ("lists"))     ;; anaphoric macros, 
		 (:file "map" :depends-on ("iteration"))          ;; map related utilities
		 (:file "iteration2" :depends-on ("map"))         ;; more iteration related utilities
		 (:file "shorthand" :depends-on ("map" "conditionals")) ;; abbreviations
		 (:file "functions" :depends-on ("shorthand"))    ;; function utilities
		 (:file "math" :depends-on ("functions"))         ;; math tools
		 (:file "setf" :depends-on ("functions"))         ;; shortcuts for dealing with places & setf macros
		 (:file "clos" :depends-on ("functions"))         ;; utilities for clos
		 (:file "hashutil" :depends-on ("shorthand"))     ;; a wrapper around the hash function
		 (:file "file" :depends-on ("shorthand"))         ;; file utilities
		 (:file "bitvector" :depends-on ("shorthand"))    ;; bitvector utilites (empty for now)
		 (:file "arrays" :depends-on ("shorthand"))       ;; Various array-oriented utilities
		 (:file "random")                                 ;; Random generation and manipulation
		 (:file "regex" 
			:depends-on ("imports" "arrays" "iteration")) ;; Regular expression enhancement
		 (:file "strings" :depends-on ("regex"))          ;; String utilities of various kinds
		 (:file "allegro")                                ;; Dependency for allegro libs
		 (:file "split-sequence")                         ;; Sequence manipulation
		 (:file "ifstar" :depends-on ("package"))         ;; John Fodero's if*
		 (:file "interactive" :depends-on ("functions" "clos")) ;; useful repl tools

		 ;; Larger scale utilities, but common enough to be included here
		 (:file "tokenizer" :depends-on ("lists"))    ;; a simple configurable tokenizer
		 (:file "match" :depends-on ("shorthand"))    ;; structure matching with variables
		 (:file "log")                                ;; simple generic logging facility for production and debugging use
;;		 (:file "plotutils")                          ;; additions to cllibs gnuplot interface
		 (:file "time" :depends-on ("shorthand"))     ;; Utility set for parsing time strings
		 (:file "prof"  :depends-on ("shorthand"))    ;; profiling support
;;		 (:file "monitor")                            ;; a perf monitoring system; package 'monitor'
		 (:file "threads")
		 (:file "matrix")

		 ;; Data structures; move to a different library later?
		 (:file "queue" :depends-on ("shorthand"))
		 (:file "cache" :depends-on ("queue" "hashutil"))
		 (:file "collections"  :depends-on ("shorthand" "clos" "hashutil")) ;; collection ds
		 (:file "wordseq"  :depends-on ("shorthand"))    ;; manipulate sequences of dictionary words
		 (:file "gds" :depends-on ("clos" "collections"))
		 (:file "table" :depends-on ("gds"))
		 (:file "iterator" :depends-on ("table"))
		 (:file "assoc-table" :depends-on ("iterator"))
		 (:file "hashed-table" :depends-on ("assoc-table"))
		 (:file "vector-keyed-table" :depends-on ("hashed-table"))
;;		 (:file "vechash"  :depends-on ("shorthand"))    ;; a fast hash table for vector keys
;;		 (:file "sbtree" :depends-on ("shorthand"))      
		 )
    :serial t
    :in-order-to ((load-op (compile-op :utils)))
    :depends-on (:cl-ppcre))