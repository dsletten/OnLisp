;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               pitfalls.lisp
;;;;
;;;;   Started:            Sat Jun 12 19:46:32 2021
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
;;;;   Notes: None of these "pitfalls" occur in any of my Common Lisps!
;;;;   ABCL, CLISP, Clozure, CMUCL, SBCL
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :pitfalls (:use :common-lisp :lang :test))

(in-package :pitfalls)

(defun et-al (&rest args)
  (nconc args (list 'et 'al)))

(defvar *greats* (list 'leonardo 'michelangelo))
(defvar *greats-literal* '(leonardo michelangelo))

(defmacro echo (&rest args)
  `',(nconc args (list 'amen)))

(defun foo ()
  (echo x))

(defmacro safe-echo (&rest args)
  `'(,@args amen))

(defun bar ()
  (safe-echo x))

(defmacro crazy (expr)
  (nconc expr (list t)))

(defun baz ()
  (crazy (list)))


