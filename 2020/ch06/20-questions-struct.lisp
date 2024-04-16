;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               20-questions-struct.lisp
;;;;
;;;;   Started:            Sun May  9 04:37:03 2021
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
;;;;   Non-terminal nodes have YES/NO branches.
;;;;   Terminal nodes simply have a value as CONTENTS.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :20-questions-struct (:use :common-lisp :lang :test))

(in-package :20-questions-struct)

(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun reset ()
  (clrhash *nodes*))

(defun defnode (name contents &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents contents
                   :yes yes
                   :no no)))

(defun run-node (name)
  (let ((node (gethash name *nodes*)))
    (cond ((node-yes node)
           (format t "~A~%>> " (node-contents node))
           (case (read)
             (yes (run-node (node-yes node)))
             (t (run-node (node-no node)))) )
          (t (node-contents node)))) )

;(load "/home/slytobias/lisp/books/OnLisp/2020/ch06/lincoln.lisp")
;(run-node 'people)
