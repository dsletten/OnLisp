;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               animals-clos.lisp
;;;;
;;;;   Started:            Tue Aug 12 21:38:56 2025
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

(make-non-terminal :name 'animals :question "Does it purr?" :yes 'purr :no 'no-purr)
(make-non-terminal :name 'purr :question "Does it run on regular gas?" :yes 'gas :no 'no-gas)
(make-terminal :name 'gas :contents 'ferrari)
(make-terminal :name 'no-gas :contents 'cat)
(make-non-terminal :name 'no-purr :question "Is it gray?" :yes 'gray :no 'not-gray)
(make-terminal :name 'gray :contents 'elephant)
(make-non-terminal :name 'not-gray :question "Is it bigger than a person?" :yes 'bigger :no 'smaller)
(make-terminal :name 'smaller :contents 'monkey)
(make-non-terminal :name 'bigger :question "Does it have stripes?" :yes 'stripes :no 'no-stripes)
(make-terminal :name 'stripes :contents 'tiger)
(make-non-terminal :name 'no-stripes :question "Is it a domestic animal?" :yes 'domestic :no 'wild)
(make-terminal :name 'domestic :contents 'horse)
(make-terminal :name 'wild :contents 'gorilla)

