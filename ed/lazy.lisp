;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               lazy.lisp
;;;;
;;;;   Started:            Mon May 23 11:21:58 2022
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

(defpackage :lazy (:use :common-lisp :lang :test))

(in-package :lazy)

;; (defmacro defunle (name args &body body)
;;   `(defun ,name ,args
;;      (let ((values (make-hash-table)))
;;            (flet (,@(map 'list
;;                   (lambda (sym)
;;                     `(,sym ()
;;                              (multiple-value-bind (value present) (gethash ',sym values)
;;                                (if present
;;                                    value
;;                                    (let ((value ,sym))
;;                                      (setf (gethash ',sym values) value)
;;                                      value)))))
;;                       args))
;;              ,@(labels
;;                  ((subsyms (syms tree)
;;                     (if syms
;;                         (let ((s (car syms)))
;;                           (subsyms (cdr syms) (subst `(,s) s tree)))
;;                         tree)))
;;                  (subsyms args body))))))

;; (defunle foole (x y z) (+ z y))
;; (foole (progn (print "x") 1) (progn (print "y") 2) (progn (print "z") 3))
;; (foo (progn (print "x") 1) (progn (print "y") 2) (progn (print "z") 3))

(defmacro defunle (name (x y z) &body body)
  (let ((subst (sublis '((x . (x)) (y . (y)) (z . (z))) body)))
    `(defmacro ,name (x y z)
       `(flet ((x () ,,x)
               (y () ,,y)
               (z () ,,z))
              ,,@subst)))) ; <-- Wrong

(defmacro defunle (name (x y z) &body body)
  (let ((subst (sublis '((x . (x)) (y . (y)) (z . (z))) body)))
    (list 'defmacro name (list 'x 'y 'z)
       (append (list 'flet (list (list 'x () x) ; <-- Wrong
                                 (list 'y () y)
                                 (list 'z () z)))
              subst))))



(DEFMACRO FOOLE (X Y Z)
  `(FLET ((X ()
           ,X)
         (Y ()
           ,Y)
         (Z ()
           ,Z))
    (+ (Z) (Y))))

Goal:
(foole (progn (print "x") 1) (progn (print "y") 2) (progn (print "z") 3)) => 
(FLET ((X ()
         (PROGN (PRINT "x") 1))
       (Y ()
         (PROGN (PRINT "y") 2))
       (Z ()
         (PROGN (PRINT "z") 3)))
  (+ (Z) (Y)))
