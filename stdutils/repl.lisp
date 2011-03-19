
(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                                 (apply #'read-line args)
                                 ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))


(defvar *console-msgs* t)
(defvar *console-msgs-types* nil)

(defun cmsg (template &rest args)
  "Format output to console"
  (when *console-msgs*
    (setq template (concatenate 'string "~&;; " template "~%"))
    (apply #'format t template args)))

(defun cmsg-c (condition template &rest args)
  "Push CONDITION keywords into *console-msgs-types* to print console msgs
   for that CONDITION.  TEMPLATE and ARGS function identically to
   (format t TEMPLATE ARGS) "
  (when (or (member :verbose *console-msgs-types*)
            (member condition *console-msgs-types*))
    (apply #'cmsg template args)))

(defun cmsg-add (condition)
  (pushnew condition *console-msgs-types*))

(defun cmsg-remove (condition)
  (setf *console-msgs-types* (remove condition *console-msgs-types*)))

(defun fixme (template &rest args)
  "Format output to console"
  (setq template (concatenate 'string "~&;; ** FIXME ** " template "~%"))
  (apply #'format t template args)
  (values))
