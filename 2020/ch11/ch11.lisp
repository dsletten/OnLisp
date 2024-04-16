;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch11.lisp
;;;;
;;;;   Started:            Fri Jun 18 13:25:25 2021
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
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch11 (:use :common-lisp :lang :test) (:shadow :let))

(in-package :ch11)

(defmacro let (bindings &body body)
  `((lambda ,(mapcar #'(lambda (binding)
                        (if (consp binding) (first binding) binding))
                    bindings)
      ,@body)
    ,@(mapcar #'(lambda (binding)
                  (if (consp binding) (second binding) nil))
              bindings)))

(defun binding-var (binding)
  (if (consp binding)
      (first binding)
      binding))

(defun binding-val (binding)
  (if (consp binding)
      (second binding)
      nil))

(defmacro let (bindings &body body)
  `((lambda ,(mapcar #'binding-var bindings)
      ,@body)
    ,@(mapcar #'binding-val bindings)))
