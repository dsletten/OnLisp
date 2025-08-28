;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               bst-network.lisp
;;;;
;;;;   Started:            Sun Aug 10 14:53:50 2025
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
;;;;   This doesn't quite work. The takeaway is that not all binary trees are binary search trees?!
;;;;   With a BST, traversing nodes involves a three-way decision--me? left child? right child?
;;;;   Each such decision is made entirely based on the current node without involving either child.
;;;;   Effectively, the current node is chosen only when both children can be ruled out.
;;;;   All nodes are homogeneous with no substantial difference between internal nodes and leaves.
;;;;
;;;;   The binary decision trees of the 20 Questions sort (and Touretzky's discrimination networks) on the other hand,
;;;;   have distinct question and answer nodes. A question node is never the end of the traversal. It can only lead
;;;;   on to other (potentially terminal) nodes.
;;;;
;;;;   Norvig's implementation is intriguingly similar to a BST. His nodes are homogeneous and pose a three-way
;;;;   choice: IT/YES/NO. Yet, the determination that the current node is "IT" rests with the user's response.
;;;;   There is no intrinsic "ordering" that could independently eliminate the YES and NO branches.
;;;;
;;;;   Thus, neither style of tree (Slade/Tatar/Touretzky vs. Norvig) is consistent with the FIND algorithm in
;;;;   my BST implementation:
;;;;     (defmethod find ((tree binary-search-tree) target)                               
;;;;       (labels ((find-node (current)                                                  
;;;;                  (cond ((null current) nil)                                          
;;;;                        ((< tree target (value current)) (find-node (left current)))  
;;;;                        ((> tree target (value current)) (find-node (right current))) 
;;;;                        (t current))))                                                
;;;;         (find-node (root tree))))
;;;;

(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Kubica/ch05/binary-search-tree.lisp")

(defpackage :bst-network (:use :common-lisp :core :io :test))

(in-package :bst-network)

(defclass question ()
  ((text :reader text :initarg :text :type string)))

(defclass answer ()
  ((value :reader value :initarg :value :type string)))


(defvar *root* (make-instance 'containers::tree-node :value (make-instance 'question :text "Does it lay eggs?")))
(defvar *left* (make-instance 'containers::tree-node :value (make-instance 'answer :value 'chicken)))
(defvar *right* (make-instance 'containers::tree-node :value (make-instance 'answer :value 'dolphin)))

(setf (slot-value *root* 'containers::left) *left*)
(setf (slot-value *root* 'containers::right) *right*)

(defvar *bst* (make-instance 'containers::mutable-binary-search-tree :test #'(lambda (a b) (declare (ignore a)) (confirm (text b)))))
(defvar *bst* (make-instance 'containers::mutable-binary-search-tree :test #'20<))

(setf (containers::root *bst*) *root*)
(setf (slot-value *bst* 'containers::count) 1)

(defun 20< (a b)
  (print (list a b))
  (etypecase b
   (question (confirm (text b)))
   (answer (eq (value b) a))))
