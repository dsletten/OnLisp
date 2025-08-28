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
;;;;   Non-terminal nodes have YES/NO branches. If a node has a question, then it has both YES and NO children.
;;;;   RUN-NODE only checks for the presence of YES node. NO node is implicit as well (OTHERWISE branch of CASE).
;;;;   
;;;;   Terminal nodes simply have a value as CONTENTS.
;;;;
;;;;   Graham uses a single structure to represent both question and answer nodes, but they are still
;;;;   distinct as with Slade, etc... (Not homogeneous as with Norvig).
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :20-questions-struct (:use :common-lisp :core :test))

(in-package :20-questions-struct)

(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun reset ()
  (clrhash *nodes*))

;;;
;;;    NAME is only used in *NODES* to tie everything together. Not part of structure.
;;;    
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
