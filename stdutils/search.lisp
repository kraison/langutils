(in-package :utils)

(defun find-function-by-name (prefix-string package &optional (type :normal))
  "Finds a function using swank's keyword completion functionality"
  (ecase type
    (:normal (swank:completions prefix-string package))
    (:fuzzy  (swank:fuzzy-completions prefix-string package))))

;; NOTE: Speed up with inverse index
(defun find-function-by-documentation (words package &aux results)
  (do-symbols (symbol (find-package package))
    (when (and (fboundp symbol) 
	       (matching-procedural-docstring-p symbol (mklist words)))
      (push symbol results)))
  (sort results
	#'(lambda (sym1 sym2)
	    (string< (symbol-name sym1)
		     (symbol-name sym2)))))
      
(defun matching-procedural-docstring-p (symbol words)
  "Look in the docstring, if any, and return symbols that contain
   all the words in the docstring"
  (some #'(lambda (string)
	    (let ((doc-words (extract-words string)))
	      (every #'(lambda (word)
			 (find word doc-words :test #'equal))
		     words)))
	(remove-nulls 
	 (list (documentation symbol 'function)
	       (documentation symbol 'macro)
	       (documentation symbol 'method)
	       (documentation symbol 'setf)))))

;; NOTE: We'll need to cache common result sets for ajax
;; NOTE: lemmatization, query expansion, goal identification,
;;       code reading, etc.
