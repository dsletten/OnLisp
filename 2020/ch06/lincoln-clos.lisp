;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               lincoln-clos.lisp
;;;;
;;;;   Started:            Sat Aug  2 22:49:23 2025
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

(in-package :20-questions-clos)

(make-non-terminal :name 'people :question "Is the person a man?" :yes 'male :no 'female)
(make-non-terminal :name 'male :question "Is he living?" :yes 'liveman :no 'deadman)
(make-non-terminal :name 'deadman :question "Was he American?" :yes 'us :no 'them)
(make-non-terminal :name 'us :question "Is he on a coin?" :yes 'coin :no 'cidence)
(make-non-terminal :name 'coin :question "Is the coin a penny?" :yes 'penny :no 'coins)
(make-terminal :name 'penny :contents 'lincoln)

