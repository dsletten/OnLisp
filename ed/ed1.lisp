;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               ed1.lisp
;;;;
;;;;   Started:            Sun Jan 23 17:56:04 2022
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

(defpackage :ed1 (:use :common-lisp :lang :test))

(in-package :ed1)

(defmacro code-generator-caller-1 (code-generator)
  `(quote ,(funcall code-generator)))

(defmacro code-generator-caller-1a (code-generator)
  `(quote ,`(funcall ,code-generator)))

(defmacro code-generator-caller-2 (code-generator)
  `(quote ,(funcall (eval code-generator))))

(defmacro run-example (symbol)
  `(,symbol (lambda () 'hello)))

;; (run-example funcall)
;; yields:
;; HELLO

;; * (macroexpand-1 '(run-example funcall))
;; (FUNCALL (LAMBDA () 'HELLO))
;; T
;; * (macroexpand-1 '(LAMBDA () 'HELLO))
;; #'(LAMBDA () 'HELLO)
;; T

;;
;; (run-example code-generator-caller-2)
;; yields:
;; HELLO



;;
;; (run-example code-generator-caller-1)
;; yields:
;; The value
;;   (LAMBDA () 'HELLO)
;; is not of type
;;   (OR FUNCTION SYMBOL)
;;    [Condition of type TYPE-ERROR]




(funcall #'(lambda () 'hello))

(funcall (lambda () 'hello))

(let ((f #'(lambda () 'hello))) (funcall f))

(let ((f (lambda () 'hello))) (funcall f))

(let ((f '(lambda () 'hello))) (funcall f))

(let ((f (eval '(lambda () 'hello)))) (funcall f))
