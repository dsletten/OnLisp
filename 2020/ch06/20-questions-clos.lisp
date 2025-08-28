;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               20-questions-clos.lisp
;;;;
;;;;   Started:            Sat Aug  2 22:39:58 2025
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
;;;;   Notes: Just a different flavor of the DEFSTRUCT version??
;;;;
;;;;   Nodes registered in *NODES* hash table upon instantiation.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :20-questions-clos (:use :common-lisp :core :io :test))

(in-package :20-questions-clos)

(defvar *nodes* (make-hash-table))

(defclass node ()
  ((name :reader name :initarg :name)))

(defmethod initialize-instance :after ((n node) &rest initargs)
  (declare (ignore initargs))           
  (setf (gethash (name n) *nodes*) n))
           
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

(defgeneric run-rode (name))
(defmethod run-node ((name symbol))
  (run-node (gethash name *nodes*)))
(defmethod run-node ((node non-terminal))
  (if (confirm (question node))
      (run-node (yes node))
      (run-node (no node))))

(defmethod run-node ((node terminal))
  (contents node))

(defun reset ()
  (clrhash *nodes*))

;(load "/home/slytobias/lisp/books/OnLisp/2020/ch06/lincoln-clos.lisp")
