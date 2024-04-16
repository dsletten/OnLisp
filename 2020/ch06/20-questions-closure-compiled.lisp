;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               20-questions-closure-compiled.lisp
;;;;
;;;;   Started:            Mon May 10 03:06:58 2021
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

(defpackage :20-questions-closure-compiled (:use :common-lisp :lang :test))

(in-package :20-questions-closure-compiled)

(defvar *nodes* '())

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (format t "Compiling ~A~%" root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
;        (error "No node for ~A." root)
        (destructuring-bind (name contents &optional yes no) node
          (declare (ignore name))
          (if yes ; Assumes NO exists when YES does.
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " contents)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () contents)))) ))

(defun reset ()
  (setf *nodes* '()))

;(load "/home/slytobias/lisp/books/OnLisp/2020/ch06/lincoln.lisp")
;(defvar *game* (compile-net 'people))
