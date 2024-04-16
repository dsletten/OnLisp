;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Mon Feb 20 19:03:42 2012
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

(defpackage :ch04 (:use :common-lisp :test) (:shadow :copy-tree))

(in-package :ch04)

(defun make-random-tree (generator)
  (if (< (random 1d0) 0.5)
      (list (funcall generator))
      (cons (make-random-tree generator)
            (make-random-tree generator))))

;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 i (incf i)))))
;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 (code-char (+ i (char-code #\A))) (incf i)))))

;;;
;;;    This mirrors my definition of COPY-TREE below
;;;    
(defun prune (pred obj)
  (cond ((null obj) '())
        ((atom (car obj)) (if (funcall pred (car obj))
                              (prune pred (cdr obj))
                              (cons (car obj) (prune pred (cdr obj)))) )
        (t (cons (prune pred (car obj))
                 (prune pred (cdr obj)))) ))

(defun prune (pred obj)
  (labels ((prune-aux (obj acc)
             (cond ((null obj) (nreverse acc))
                   ((atom (car obj)) (prune-aux (cdr obj)
                                                (if (funcall pred (car obj))
                                                    acc
                                                    (cons (car obj) acc))))
                   (t (prune-aux (cdr obj)
                                 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

(defun prune-if-not (pred obj)
  (labels ((prune-aux (obj acc)
             (cond ((null obj) (nreverse acc))
                   ((atom (car obj)) (prune-aux (cdr obj)
                                                (if (funcall pred (car obj))
                                                    (cons (car obj) acc)
                                                    acc)))
                   (t (prune-aux (cdr obj)
                                 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

;;;
;;;    My earlier version
;;;    
(defun copy-tree (obj)
  (cond ((null obj) '()) ; ((atom obj) obj)
        ((atom (car obj)) (cons (car obj) (copy-tree (cdr obj))))
        (t (cons (copy-tree (car obj))
                 (copy-tree (cdr obj)))) ))

;;;
;;;    Clozure 1.5 COPY-TREE
;;;    clozure/ccl_1.5/level-1/l1-aprims.lisp
;;;    
(defun copy-tree (tree)
  "Recursively copy trees of conses."
  (if (atom tree)
    tree
    (locally (declare (type cons tree))
      (do* ((tail (cdr tree) (cdr tail))
            (result (cons (copy-tree (car tree)) nil))
            (ptr result (cdr ptr)))
           ((atom tail)
            (setf (cdr ptr) tail)
            result)
        (declare (type cons ptr result))
        (locally 
          (declare (type cons tail))
          (setf (cdr ptr) (cons (copy-tree (car tail)) nil)))))))

;; Streamlined
(defun copy-tree (tree)
  (if (atom tree)
      tree
      (do* ((tail (cdr tree) (cdr tail))
            (result (cons (copy-tree (car tree)) nil))
            (ptr result (cdr ptr)))
           ((atom tail) (setf (cdr ptr) tail) result)
        (setf (cdr ptr) (cons (copy-tree (car tail)) nil)))) )

;;;
;;;    SBCL 1.0.43
;;;    sbcl/sbcl-1.0.43/src/code/list.lisp
;;;    
(defun copy-tree (object)
;  #!+sb-doc
  "Recursively copy trees of conses."
  (if (consp object)
      (cons (copy-tree (car object)) (copy-tree (cdr object)))
      object))

;; (defun prune (object)
;;   (cond ((consp object) 

(defun prune (pred object)
  (if (consp object)
      (loop for elt in object
            if (atom elt)
              unless (funcall pred elt)
                collect elt
              end
            else collect (prune pred elt))
      object))

(defun prune-if-not (pred object)
  (if (consp object)
      (loop for elt in object
            if (atom elt)
              when (funcall pred elt)
                collect elt
              end
            else collect (prune pred elt))
      object))

;;;
;;;    This version is shaped like PRUNE above.
;;;    It performs fewer recursive calls than the version below.
;;;    
(defun copy-tree (object)
  (if (consp object)
      (loop for elt in object
            if (atom elt) collect elt
            else collect (copy-tree elt))
      object))

;;;
;;;    This behaves the same as the Clozure version.
;;;    
(defun copy-tree (object)
  (if (consp object)
      (loop for elt in object
            collect (copy-tree elt))
      object))

