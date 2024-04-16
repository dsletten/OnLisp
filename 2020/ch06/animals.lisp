;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               animals.lisp
;;;;
;;;;   Started:            Tue May 11 02:29:01 2021
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

(defnode 'animals "Does it purr?" 'purr 'no-purr)
(defnode 'purr "Does it run on regular gas?" 'gas 'no-gas)
(defnode 'gas 'ferrari)
(defnode 'no-gas 'cat)
(defnode 'no-purr "Is it gray?" 'gray 'not-gray)
(defnode 'gray 'elephant)
(defnode 'not-gray "Is it bigger than a person?" 'bigger 'smaller)
(defnode 'smaller 'monkey)
(defnode 'bigger "Does it have stripes?" 'stripes 'no-stripes)
(defnode 'stripes 'tiger)
(defnode 'no-stripes "Is it a domestic animal?" 'domestic 'wild)
(defnode 'domestic 'horse)
(defnode 'wild 'gorilla)

