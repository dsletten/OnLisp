;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               scope.lisp
;;;;
;;;;   Started:            Sun Jan 30 17:35:08 2022
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

(defpackage :scope (:use :common-lisp :lang :test))

(in-package :scope)



(defun f () x)

(f 9)

(let ((x 9))
  (f))

(let ((x 9))
  (declare (special x))
  (f))


(defun g ()
  (symbol-value 'x))

(let ((x 9))
  (g))

(let ((x 9))
  (declare (special x))
  (g))




(defvar *x* 8)

(macroexpand-1 '(defvar *x* 8))
(LET NIL (PROCLAIM '(SPECIAL *X*)) (UNLESS (BOUNDP '*X*) (SYSTEM::SET-SYMBOL-VALUE '*X* 8)) '*X*) ;
T

BOUNDP/FBOUNDP





(let ((x 1)
      (y (* x 2)))
  (list x y))

(let ((x 1))
  (let ((y (* x 2)))
    (list x y)))

(let* ((x 1)
       (y (* x 2)))
  (list x y))









(defun add (x y) (+ x y))

(flet ((add (x y)
         (if (zerop y)
             (format nil "~D" x)
             (add (1+ x) (1- y)))) )
  (print (add 3 2))
  (print (add 3 0)))

(flet ((add (x y)
         (if (zerop y)
             (format nil "~D" x)
             (plus (1+ x) (1- y)))) )
  (print (add 3 2))
  (print (add 3 0)))

(flet ((add (x y)
         (if (zerop y)
             (format nil "~D" x)
             (plus (1+ x) (1- y)))) )
  #'add)
(function add)
'add
(symbol-function 'add)


'a
(quote a)

#'f
(function f)

(labels ((add (x y)
         (if (zerop y)
             (format nil "~D" x)
             (add (1+ x) (1- y)))) )
  (print (add 3 2))
  (print (add 3 0)))

(trace add)




(defun employee (name age salary)
  (list #'(lambda () name)
        #'(lambda (new-name) (setf name new-name))
        #'(lambda () age)
        #'(lambda (new-age) (setf age new-age))
        #'(lambda () salary)
        #'(lambda (new-salary) (setf salary new-salary))))







(defun counter (n)
  #'(lambda ()
      (incf n)))




(defun counter (n)
  (flet ((f ()
           (incf n)))
;    f
;    (symbol-function 'f)
    #'f))





(defclass counter ()
  ((i :initform 0 :initarg :start)))

(defmethod click ((c counter))
  (with-slots (i) c
    (incf i)))
