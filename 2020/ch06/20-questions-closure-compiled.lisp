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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :20-questions-closure-compiled (:use :common-lisp :core :io :test))

(in-package :20-questions-closure-compiled)

;; (defun compile-net (root)
;;   (format t "Compiling ~A~%" root)
;;   (let ((node (assoc root *nodes*)))
;;     (if (null node)
;;         nil
;; ;        (error "No node for ~A." root)
;;         (destructuring-bind (name contents &optional yes no) node
;;           (declare (ignore name))
;;           (if yes ; Assumes NO exists when YES does.
;;               (let ((yes-fn (compile-net yes))
;;                     (no-fn (compile-net no)))
;;                 #'(lambda ()
;;                     (format t "~A~%>> " contents)
;;                     (funcall (if (eq (read) 'yes)
;;                                  yes-fn
;;                                  no-fn))))
;;               #'(lambda () contents)))) ))


(defvar *nodes* '())

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun make-prompt (s)
  (format nil "~A~%>>" s))

(defun compile-net (root)
  (format t "Compiling ~A~%" root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil ; Only for incomplete branches, e.g., LIVEMAN
        (destructuring-bind (name contents &optional yes no) node
          (declare (ignore name))
          (cond ((and (null yes) (null no)) #'(lambda () contents))
                ((or (null yes) (null no)) (error "Missing child node."))
                (t (let ((yes-fn (compile-net yes)) ; <---- Must evaluate now rather than later!
                         (no-fn (compile-net no)))
                     #'(lambda ()
                         (if (confirm (make-prompt contents))
                             (funcall yes-fn)
                             (funcall no-fn)))) )))) ))

;;;
;;;    This incorrectly compiles the network at runtime!
;;;    另见 PROCESS-NODE in network.lisp
;;;    
;; (defun compile-net (root)
;;   (format t "Compiling ~A~%" root)
;;   (let ((node (assoc root *nodes*)))
;;     (if (null node)
;;         nil ; Only for incomplete branches, e.g., LIVEMAN
;;         (destructuring-bind (name contents &optional yes no) node
;;           (declare (ignore name))
;;           (cond ((and (null yes) (null no)) #'(lambda () contents))
;;                 ((or (null yes) (null no)) (error "Missing child node."))
;;                 (t #'(lambda ()
;;                        (if (confirm (make-prompt contents))
;;                            (funcall (compile-net yes))                <----
;;                            (funcall (compile-net no)))) )))) ))       <----



(defun reset ()
  (setf *nodes* '()))

;(load "/home/slytobias/lisp/books/OnLisp/2020/ch06/lincoln.lisp")

;; (defvar *game* (compile-net 'people))
;; Compiling PEOPLE
;; Compiling MALE
;; Compiling LIVEMAN
;; Compiling DEADMAN
;; Compiling US
;; Compiling COIN
;; Compiling PENNY
;; Compiling COINS
;; Compiling CIDENCE
;; Compiling THEM
;; Compiling FEMALE
;; *GAME*

;; (funcall *game*)
;; Is the person a man?
;; >> y
;; Is he living?
;; >> n
;; Was he American?
;; >> yep
;; Was he American?
;; >> y
;; Is he on a coin?
;; >> y
;; Is the coin a penny?
;; >> y
;; LINCOLN

;; ;;;
;; ;;;    Compile part of network
;; ;;;    
;; (defvar *us* (compile-net 'us))
;; Compiling US
;; Compiling COIN
;; Compiling PENNY
;; Compiling COINS
;; Compiling CIDENCE
;; *US*
;; (funcall *us*)
;; Is he on a coin?
;; >> y
;; Is the coin a penny?
;; >> y
;; LINCOLN


#|
(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

((PENNY LINCOLN)
 (COIN "Is the coin a penny?" PENNY COINS)
 (US "Is he on a coin?" COIN CIDENCE)
 (DEADMAN "Was he American?" US THEM)
 (MALE "Is he living?" LIVEMAN DEADMAN)
 (PEOPLE "Is the person a man?" MALE FEMALE))

((PENNY . (LINCOLN))
 (COIN . ("Is the coin a penny?" PENNY COINS))
 (US . ("Is he on a coin?" COIN CIDENCE))
 (DEADMAN . ("Was he American?" US THEM))
 (MALE . ("Is he living?" LIVEMAN DEADMAN))
 (PEOPLE . ("Is the person a man?" MALE FEMALE)))
|#
