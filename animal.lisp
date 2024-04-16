;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               animal.lisp
;;;;
;;;;   Started:            Sat Oct 15 03:53:25 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :animal (:use :common-lisp :test))

(in-package :animal)

(defclass animal () ())
(defgeneric behave (animal))
(defmethod behave ((a animal))
  (write-line "Urp!"))

(defclass dog (animal) ())
(defmethod behave ((d dog))
  (wag-tail d)
  (bark d))

(defgeneric wag-tail (dog))
(defgeneric bark (dog))

(defmethod wag-tail ((d dog))
  (write-line "Wag!"))
(defmethod bark ((d dog))
  (write-line "Woof!"))

(defclass mulgara (animal) ())
