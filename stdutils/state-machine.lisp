;;;===========================================================================
;;;
;;; project:    robot
;;; file:       state-machine.lisp
;;; author:     Andrew Smith
;;; created:    2006-08-15
;;; updated:    2006-08-20
;;; language:   Common Lisp (CLISP)
;;; licence:    GPL version 2
;;;
;;; Copyright 2006, Andrew Smith <http://asmith.id.au>
;;;
;;; NOTES
;;;
;;; Classes and methods supporting the implementation of virtual finite
;;; state machines.
;;;
;;;
;;;---------------------------------------------------------------------------
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;;
;;;---------------------------------------------------------------------------

(in-package :stdutils)

;;; class definitions --------------------------------------------------------

(defclass machine ()
  ((name :accessor name :initarg :name :initform nil)
   (state :accessor state :initarg :state :initform nil)
   (states :accessor states :initarg :states :initform nil)
   (inputs :accessor inputs :initarg :inputs :initform nil)
   (outputs :accessor outputs :initarg :outputs :initform nil)
   (counter :accessor counter :initarg :counter :initform 0)))

(defclass state ()
  ((name :accessor name :initarg :name)
   (output :accessor output :initarg :output :initform nil)
   (counter :accessor counter :initarg :counter :initform 0)
   (handlers :accessor handlers :initarg :handlers :initform nil)))


;;; initialisation -----------------------------------------------------------

;;; build a state machine from a template

(defmacro make-machine (name &rest states)
  `(make-instance 'machine :name ',name :states ',states))


;;; install a state definition in a machine

(defmethod add-state ((machine machine) (state state))
  (push state (states machine))
  (unless (state machine)
    (setf (state machine) state)))

(defmethod make-state ((machine machine) name output &rest handlers)
  (add-state machine (make-instance 'state
                                    :name name
                                    :output output
                                    :handlers handlers)))

;;; class initialisation

(defmethod initialize-instance :after ((machine machine) &rest initargs)
  (declare (ignore initargs))
  (let ((states (states machine)))
    (setf (states machine) nil)
    (dolist (state states)
      (apply #'make-state machine state))))


;;; print methods ------------------------------------------------------------

(defmethod print-object ((machine machine) stream)
  (prin1 (list (class-name (class-of machine))
               :name (name machine)
               :state (when (state machine) (name (state machine)))
               :states (states machine)
               :inputs (inputs machine)
               :outputs (outputs machine)
               :counter (counter machine))
         stream)
  machine)


(defmethod print-object ((state state) stream)
  (prin1 (list (class-name (class-of state))
               :name (name state)
               :output (output state)
               :counter (counter state)
               :handlers (handlers state))
         stream)
  state)


;;; interpreter methods ------------------------------------------------------

;;; save inputs and outputs

(defmethod add-input ((machine machine) input)
  (when input
    (first (push (list (counter machine) input) (inputs machine)))))

(defmethod input ((machine machine))
  (let ((input (first (inputs machine))))
    (when (and input (= (counter machine) (first input)))
      (second input))))


(defmethod add-output ((machine machine) output)
  (first (push (list (counter machine) (name (state machine)) output)
               (outputs machine))))

(defmethod output ((machine machine))
  (third (first (outputs machine))))


;;; process output from the state machine

(defmethod handle-output ((machine machine) output)
  (add-output machine output))


;;; jump to a new state

(defmethod new-state ((machine machine) (name symbol))
  (let ((state (or (find name (states machine) :key #'name)
                   (error "invalid state ~A" name))))
    (new-state machine state)))

(defmethod new-state ((machine machine) (state state))
  (setf (state machine) state
        (counter state) 0)
  (handle-output machine (output state)))


;;; input recognisers

(defmethod invoke-recogniser ((input t) (value t) (state symbol))
  (when (eq input value) state))

(defmethod invoke-recogniser ((input t) (values list) (state symbol))
  (when (member input values) state))

(defmethod invoke-recogniser ((inputs list) (value symbol) (state symbol))
  (when (member value inputs) state))

(defmethod invoke-recogniser ((inputs list) (values list) (state symbol))
  (when (intersection inputs values) state))


;;; event handlers

(defmethod invoke-handler ((machine machine) (state state) (handler list))
  (or (invoke-recogniser (input machine) (first handler) (second handler))
      (invoke-recogniser (counter state) (first handler) (second handler))))

(defmethod invoke-handler ((machine machine) (state state) (handler symbol))
  (identity handler))

(defmethod invoke-handler ((machine machine) (state state) (handler function))
  (funcall handler machine))


;;; find the first applicable event handler of the given state

(defmethod handle-input ((machine machine) (state state))
  (incf (counter state))
  (let ((name (some #'(lambda (handler)
                        (invoke-handler machine state handler))
                    (handlers state))))
    (when name
      (new-state machine name))))


;;; handle input in the current state

(defmethod handle-input ((machine machine) input)
  (incf (counter machine))
  (add-input machine input)
  (handle-input machine (state machine)))


;;; feed a sequence of inputs to the machine at specified times

(defmethod handle-script ((machine machine) script)
  (dolist (event script)
    (let ((due (first event))
          (input (second event)))
      (when (< due (counter machine))
        (error "time due ~A is less than current time ~A" due (counter machine)))
      (loop until (= due (counter machine))
            do (handle-input machine nil)
            finally (handle-input machine input)))))


;;; generate drawing ---------------------------------------------------------

(defmethod extract-nodes ((machine machine))
  (nreverse (mapcar #'name (states machine))))

(defmethod extract-edges ((machine machine))
  (mapcan #'extract-edges (states machine)))

(defmethod extract-edges ((state state))
  (mapcar #'(lambda (handler)
              (cons (name state) (extract-edges handler)))
          (handlers state)))

(defmethod extract-edges ((handler t))
  (list nil handler))

(defmethod extract-edges ((handler list))
  (list (edge-label (first handler)) (second handler)))


(defmethod edge-label ((label symbol))
  (string-downcase label))

(defmethod edge-label ((timeout integer))
  (format nil "t=~A" timeout))

(defmethod edge-label ((labels list))
  (format nil "~(~{~A~^,\\n~}~)" labels))


(defmethod draw-name (name)
  (format t "~(digraph \"~A\"~)~%" name))

(defmethod draw-node (node active)
  (format t "~4T~(\"~A\"~)~@[[style=bold]~];~%" node active))

(defmethod draw-edge (node1 edge node2)
  (format t "~4T~(\"~A\"->\"~A\"~@[[label=\"~A\"]~]~);~%" node1 node2 edge))


(defmethod draw-machine ((machine machine))
  (let ((name (name machine))
        (nodes (extract-nodes machine))
        (edges (extract-edges machine)))
    (draw-name name)
    (format t "{~%~4Tcenter=TRUE;~%")
    (format t "~4Trankdir=LR;~%")
    (format t "~4Tpage=\"8.26,11.69\";~%")
    (format t "~4Tmargin=\"0.2,0.2\";~%")
    (format t "~4Tsize=\"11.2,7.8\";~%")
    (format t "~4Tbgcolor=ivory;~%")
    (format t "~4Tnode[color=green,fontcolor=green];~%")
    (format t "~4Tedge[color=green,fontcolor=green];~%")
    (terpri)
    (dolist (node nodes)
      (draw-node node (eq node (name (state machine)))))
    (terpri)
    (dolist (edge edges)
      (draw-edge (first edge) (second edge) (third edge)))
    (format t "}")))


;;;---------------------------------------------------------------------------

(defvar *ally*
  (make-machine ally
    (start nil (1 forward))
    (forward :forward (:left-bumper left-stop) (:right-bumper right-stop))
    (left-stop :stop (1 left-reverse))
    (right-stop :stop (1 right-reverse))
    (left-reverse :reverse (2 turn-right))
    (right-reverse :reverse (2 turn-left))
    (turn-right :turn-right (1 left-forward))
    (turn-left :turn-left (1 right-forward))
    (left-forward :forward ((:left-bumper :right-bumper) left-stop) (8 forward))
    (right-forward :forward ((:left-bumper :right-bumper) right-stop) (8 forward))))


;;;---------------------------------------------------------------------------

;;; (print *ally*)

#|
(handle-script *ally* '((4 :left-bumper)(12 :right-bumper)(50 nil)))

(format t "~4<TIME~>~4TSTATE~20TOUTPUT~%")
(dolist (line (reverse (outputs *ally*)))
  (format t "~(~?~)" "~4<~A~>~4T~A~20T~A~%" line))
|#

;;; (draw-machine *ally*)

;;;---------------------------------------------------------------------------


;;;===========================================================================
