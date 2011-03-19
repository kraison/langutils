(in-package :stdutils)

;; -----------------------------------
;; Simple tokenizer (from Paul Graham)
;; -----------------------------------

(defun-exported extract-tokens (str test &optional (start 0))
  "Returns a list of the subsequences satisfying test, characters
   failing the test are treated as whitespace."
  (declare (optimize (speed 3) (safety 0) (debug 1))
	   (type string str)
	   (type fixnum start))
  (let ((p1 (position-if test str :start start)))
    (declare (type (or null fixnum) p1))
    (when p1
      (let ((p2 (position-if-not test str :start p1)))
	(declare (type (or null fixnum) p2))
	(if p2
	    (cons (subseq str p1 p2)
		  (extract-tokens str test p2))
	    (cons (subseq str p1) nil))))))

(defun-exported constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\Space ))))

;; Custom detokenizer to extract words by
;; whitespace separation.
(defun-exported extract-words (str &optional (start 0))
  (extract-tokens str #'constituent start))

;; Special detokenizer, splits sentences like
;; The/DE Bird/NP Flew/VP to/PP the/DE tree/NP
;; into '("The" "Bird" "Flew" "to" "the" "tree")
;;  and '("DE" "NP" "VP" "PP" "DE" "NP")
;; as two valued result

(defun-exported extract-tagged-words (tagspec sentence)
  (let (a b)
    (mapc #'(lambda (tok)
	      (let ((res (extract-tokens 
			  tok
			  #'(lambda (x) (not (find x tagspec))))))
		(setf a (cons (first res) a))
		(setf b (cons (second res) b))))
	  (extract-words sentence))
    (values (nreverse a) (nreverse b))))

;; Convert a string of bracketed expressions such as
;; that described by the regex: [".*"]* to individual
;; strings of all data between two sets of quotes
(defun-exported quote-tokenize (string)
  (let ((length (length string))
	(res nil))
    (print string)
    (do* ((p1 (position #\" string) 
	      (position #\" string :start (1+ p2)))
	  (p2 (position #\" string :start (1+ p1)) 
	      (if p1 (position #\" string :start (1+ p1)) (setf p2 length))))
	((>= p2 length))
      (print (subseq string (1+ p1) p2))
      (push (subseq string (1+ p1) p2) res))
    (nreverse res)))
