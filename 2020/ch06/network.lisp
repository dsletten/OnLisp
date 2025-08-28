;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               network.lisp
;;;;
;;;;   Started:            Sun Aug  3 23:50:22 2025
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes: No registry of nodes (*NODES*). Entire network is built up from leaves.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :network (:use :common-lisp :core :test :io))

(in-package :network)

(defclass node ()
  ((name :reader name :initarg :name)))

(defclass non-terminal (node)
  ((question :reader question :initarg :question)
   (yes :reader yes :initarg :yes)
   (no :reader no :initarg :no)))

(defclass terminal (node)
  ((contents :reader contents :initarg :contents)))

(defun make-non-terminal (&key name question yes no)
  (make-instance 'non-terminal :name name :question question :yes yes :no no))

(defun make-terminal (&key name contents)
  (make-instance 'terminal :name name :contents contents))

(defgeneric run-rode (node))
(defmethod run-node ((node non-terminal))
  (if (confirm (question node))
      (run-node (yes node))
      (run-node (no node))))

(defmethod run-node ((node terminal))
  (contents node))

(defun process-node (node)
  (if (null node)
      nil
      (destructuring-bind (name contents &optional yes no) node
        (cond ((and (null yes) (null no)) (make-terminal :name name :contents contents))
;              ((or (null yes) (null no)) (error "Missing branch"))
              (t (make-non-terminal :name name
                                    :question contents
                                    :yes (process-node yes)
                                    :no (process-node no)))) )))

;(defmacro defnet (network node)

;; (run-node (process-node '(people "Is the person a man?"
;;                                  (male "Is he living?" 
;;                                        (liveman nil)
;;                                        (deadman "Was he American?"
;;                                                 (us "Is he on a coin?"
;;                                                     (coin "Is the coin a penny?"
;;                                                           (penny lincoln)
;;                                                           (coins nil))
;;                                                     (cidence nil))
;;                                                 (them nil)))
;;                                  (female nil))))
