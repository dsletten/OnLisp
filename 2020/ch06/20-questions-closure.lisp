;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               20-questions-closure.lisp
;;;;
;;;;   Started:            Sun May  9 04:43:29 2021
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
;;;;   Notes: Nodes are not structures but rather functions (closures).
;;;;
;;;;   Representation is more opaque!
;;;;   (describe (gethash 'people *nodes*))                                              
;;;;   #<FUNCTION (LAMBDA () :IN DEFNODE) {1003DC7E0B}>                                  
;;;;     [compiled closure]                                                              
;;;;                                                                                     
;;;;                                                                                     
;;;;   Lambda-list: ()                                                                   
;;;;   Derived type: (FUNCTION NIL *)                                                    
;;;;   Documentation:                                                                    
;;;;     T                                                                               
;;;;   Source file: /home/slytobias/lisp/books/OnLisp/2020/ch06/20-questions-closure.lisp
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :20-questions-closure (:use :common-lisp :core :io :test))

(in-package :20-questions-closure)

(defvar *nodes* (make-hash-table))

;; (defun defnode (name contents &optional yes no)
;;   (setf (gethash name *nodes*)
;;         (if yes
;;             #'(lambda ()
;;                 (format t "~A~%>> " contents)
;;                 (case (read)
;;                   (yes (funcall (gethash yes *nodes*)))
;;                   (t (funcall (gethash no *nodes*)))) )
;;             #'(lambda () contents))))


(defun make-prompt (s)
  (format nil "~A~%>>" s))

;(defun leafp (node)
(defun defnode (name contents &optional yes no)
  (setf (gethash name *nodes*)
        (cond ((and yes no)
               #'(lambda ()
                   (if (confirm (make-prompt contents))
                       (funcall (gethash yes *nodes*))
                       (funcall (gethash no *nodes*)))) )
              ((or yes no) (error "Missing branch"))
              (t #'(lambda () contents)))) )

(defun reset ()
  (clrhash *nodes*))

;(load "/home/slytobias/lisp/books/OnLisp/2020/ch06/lincoln.lisp")
;(funcall (gethash 'people *nodes*))

;;;
;;;    Support separate tables.
;;;    
(defun defnodes (table)
  #'(lambda (command &optional key value)
      (case command
        (get (gethash key table))
        (set (setf (gethash key table) value)))) )

(defun read-node (nodes key)
  (funcall nodes 'get key))

(defun set-node (nodes key val)
  (funcall nodes 'set key val))





