;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;    -- Alan Perlis
;;;;
;;;;    Name:               for.lisp
;;;;
;;;;    Started:            Thu Aug 28 22:15:03 2025
;;;;    Modifications:
;;;;
;;;;    Purpose:
;;;;
;;;;
;;;;
;;;;    Calling Sequence:
;;;;
;;;;
;;;;    Inputs:
;;;;
;;;;    Outputs:
;;;;
;;;;    Example:
;;;;
;;;;    Notes:
;;;;    Artificial Intelligence Programming 2e Charniak, Riesbeck, McDermott, Mehan §3.12-3.13
;;;;    Common LISPcraft, Robert Wilensky §13.9
;;;;
;;;;    The `for' macro consolidates (simplified versions of) the various mapping functions of
;;;;    Common Lisp specified here:
;;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm
;;;;
;;;;    The 6 functions MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, MAPCON can be categorized by how
;;;;    they traverse a list and what kind of result they return.
;;;;
;;;;    3 of the functions traverse successive CARs of a list operating on the elements of
;;;;    said list: MAPC, MAPCAR, MAPCAN. These are represented by the IN operations in FOR.
;;;;
;;;;    The other 3 traverse successive CDRs, exposing each tail of the list: MAPL, MAPLIST, MAPCON.
;;;;    These are represented by ON operations in FOR.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

#|
;;;
;;;    AIP examples 69 页
;;;
(for (x :in l) :do (foo x)) =>     (mapc #'foo l) ; Discards user-specified variable X!  Not the actual expansion?!?!
(for (x :in l) :save (foo x)) =>   (mapcar #'foo l)
(for (x :in l) :splice (foo x)) => (mapcan #'foo l)
(for (x :in l) :filter (foo x)) => (mapcan #'(lambda (x) (let ((x (foo x))) (if x (list x) nil))) l) ; This is Graham's ch. 4 FILTER

(for (x :in l) :when (foo x) :do (baz x)) =>     (mapc #'(lambda (x) (if (foo x) (baz x) nil)) l)
(for (x :in l) :when (foo x) :save x) =>         (remove-if-not #'foo l) ; Normal FILTER!
(for (x :in l) :when (foo x) :save (baz x)) =>   (mapcan #'(lambda (x) (if (foo x) (list (baz x)) nil)) l)
(for (x :in l) :when (foo x) :splice (baz x)) => (mapcan #'(lambda (x) (if (foo x) (baz x) nil)) l)
(for (x :in l) :when (foo x) :filter (baz x)) => (mapcan #'(lambda (x) (if (foo x) (let ((x (baz x))) (if x (list x) nil)) nil)) l)

(for (x :in l1) (y :in l2) :filter (+ x y))  Multiple vars/lists


Actual expansions!
(macroexpand-1 '(for (x :in l) :do (foo x)))
(MAPC #'(LAMBDA (X) (FOO X)) L)

(macroexpand-1 '(for (x :in l) :save (foo x)))
(MAPCAR #'(LAMBDA (X) (FOO X)) L)

(macroexpand-1 '(for (x :in l) :splice (foo x)))
(MAPCAN #'(LAMBDA (X) (FOO X)) L)

(macroexpand-1 '(for (x :in l) :filter (foo x)))
(MAPCAN
 #'(LAMBDA (X)
     (LET ((X (FOO X)))
       (IF X
           (LIST X)
           NIL)))
 L)



(macroexpand-1 '(for (x :in l) :when (foo x) :do (baz x)))
(MAPC
 #'(LAMBDA (X)
     (IF (FOO X)
         (BAZ X)
         NIL))
 L)

(macroexpand-1 '(for (x :in l) :when (foo x) :save x))
(REMOVE-IF-NOT #'(LAMBDA (X) (FOO X)) L)

(macroexpand-1 '(for (x :in l) :when (foo x) :save (baz x)))
(MAPCAN
 #'(LAMBDA (X)
     (IF (FOO X)
         (LIST (BAZ X))
         NIL))
 L)

(macroexpand-1 '(for (x :in l) :when (foo x) :splice (baz x)))
(MAPCAN
 #'(LAMBDA (X)
     (IF (FOO X)
         (BAZ X)
         NIL))
 L)

(macroexpand-1 '(for (x :in l) :when (foo x) :filter (baz x)))
(MAPCAN
 #'(LAMBDA (X)
     (IF (FOO X)
         (LET ((X (BAZ X)))
           (IF X
               (LIST X)
               NIL))
         NIL))
 L)

(macroexpand-1 '(for (x :in l1) (y :in l2) :filter (+ x y)))
(MAPCAN
 #'(LAMBDA (X Y)
     (LET ((X (+ X Y)))
       (IF X
           (LIST X)
           NIL)))
 L1 L2)



(for (x :in '(1 2 3)) (y :in '(2 4 6)) :save (+ x y))
(3 6 9)
(macroexpand-1 '(for (x :in '(1 2 3)) (y :in '(2 4 6)) :save (+ x y)))
(MAPCAR #'(LAMBDA (X Y) (+ X Y)) '(1 2 3) '(2 4 6))
|#

(defpackage :for-aip2 (:use :common-lisp :core :test) (:shadow :for))

(in-package :for-aip2)

(defmacro for (&rest l) ; Clauses can be in any order (including multiple :IN clauses: FOR-VARS/FOR-ARGS)
  (let ((vars (for-vars l))
        (args (for-args l))
        (test (for-test l))
        (type (for-type l))
        (body (for-body l)))
    `(,(make-mapfn vars test type body)
       #',(make-lambda vars (add-test test (make-body vars test type body)))
       ,@args)))

;; (defun for-vars (l)
;;   (mapcan #'(lambda (x)
;;               (if (var-form-p x) (list (var-form-var x)) nil))
;;           l))

(defun for-vars (l)
  (loop for elt in l if (var-form-p elt) collect (var-form-var elt)))

(defun var-form-p (x)
  (and (consp x)
       (= (length x) 3)
       (eq (cadr x) :in)))

(defun var-form-var (x) (car x))
(defun var-form-arg (x) (caddr x))

;; (defun for-args (l)
;;   (mapcan #'(lambda (x)
;;               (if (var-form-p x) (list (var-form-arg x)) nil))
;;           l))

(defun for-args (l)
  (loop for elt in l if (var-form-p elt) collect (var-form-arg elt)))

(defun for-test (l)
  (cadr (for-item '(:when) l)))

(defun for-type (l)
  (let ((item (for-item '(:do :save :splice :filter) l)))
    (cond (item (car item))
          (t (error "No body in FOR-loop")))) )

(defun for-body (l)
  (let ((item (for-item '(:do :save :splice :filter) l)))
    (cond (item (cadr item))
          (t (error "No body in FOR-loop")))) )

;;;
;;;    Only first `type' detected by ordering of the list above is considered.
;;;    Subsequent `type's are ignored. In other words, :DO supersedes :SAVE, which supersedes :SPLICE, etc...
;;;    (macroexpand-1 '(for (x :in l) :do (foo x)))                    <-- Identical
;;;    (MAPC #'(LAMBDA (X) (FOO X)) L)                                                           
;;;                                                                                              
;;;    (macroexpand-1 '(for (x :in l) :save (foo x)))                                            
;;;    (MAPCAR #'(LAMBDA (X) (FOO X)) L)                                                         
;;;                                                                                              
;;;    (macroexpand-1 '(for (x :in l) :do (foo x) :save (foo x)))      <-- Identical             
;;;    (MAPC #'(LAMBDA (X) (FOO X)) L)
;;;
;;;    (macroexpand-1 '(for (x :in l) :save (foo x) :do (foo x)))      <-- Identical
;;;    (MAPC #'(LAMBDA (X) (FOO X)) L)
;;;                                                                                              
;;;    (macroexpand-1 '(for (x :in l) :do (foo x) :save (foo x) :splice (foo x)))                
;;;    (MAPC #'(LAMBDA (X) (FOO X)) L)                                                           
;;;                                                                                              
;;;    (macroexpand-1 '(for (x :in l) :do (foo x) :save (foo x) :splice (foo x) :filter (foo x)))
;;;    (MAPC #'(LAMBDA (X) (FOO X)) L)                                                           
;;;
;;;
;;;    Malformed:
;;;    (macroexpand-1 '(for (x :in l) :do :save))
;;;    (MAPC #'(LAMBDA (X) :SAVE) L)             
;;;

(for (x :in '(1 2 3)) :save :do)
; in: FOR (X :IN '(1 2 3))
;     (LAMBDA (FOR-AIP2::X) NIL)
; 
; caught STYLE-WARNING:
;   The variable X is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
(1 2 3)
(macroexpand-1 '(for (x :in '(1 2 3)) :save :do))
(MAPC #'(LAMBDA (X) NIL) '(1 2 3))

(macroexpand-1 '(for (x :in '(1 2 3)) :save :do))
(MAPC #'(LAMBDA (X) NIL) '(1 2 3))

(for (x :in '(1 2 3)) :save :splice)
; in: FOR (X :IN '(1 2 3))
;     (LAMBDA (FOR-AIP2::X) :SPLICE)
; 
; caught STYLE-WARNING:
;   The variable X is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
(:SPLICE :SPLICE :SPLICE)

(macroexpand-1 '(for (x :in '(1 2 3)) :save :splice))
(MAPCAR #'(LAMBDA (X) :SPLICE) '(1 2 3))




;; (defun for-item (keywords l)
;;   (some #'(lambda (key) (member key l)) keywords))

(defun for-item (keywords l)
  (some (partial* #'member l) keywords))

(defun make-mapfn (vars test type body)
  (cond ((eq type :do) 'mapc)
        ((not (eq type :save)) 'mapcan) ; TYPE not :DO or :SAVE
        ((null test) 'mapcar) ; TYPE is :SAVE
        ((use-remove-if-not-p vars body) 'remove-if-not)
        (t 'mapcan)))

(defun use-remove-if-not-p (vars body)
  (and (= (length vars) 1) (equal (car vars) body)))

(defun make-body (vars test type body)
  (cond ((eq type :filter) `(let ((x ,body)) (if x (list x) nil))) ; Shadow
        ((or (not (eq type :save)) (null test)) body)
        ((use-remove-if-not-p vars body) nil)
        (t `(list ,body))))

(defun add-test (test body)
  (cond ((null test) body)
        ((null body) test)
        (t `(if ,test ,body nil))))

(defun make-lambda (vars body)
  `(lambda ,vars ,body))














(defpackage :for (:use :common-lisp :core :test))

(in-package :for)

(defmacro for ((elt traverse l) &key ((when when)) ((do do)) ((save save)) ((splice splice)) ((filter filter)))
 (ecase traverse
    (in (for-in elt l when do save splice filter))
    (on (for-on elt l))))

;(defmacro for ((elt traverse l) &key ((:when when)) ((:do do)) ((:save save)) &body body)
 ;; (ecase traverse
 ;;    (in (for-in elt l when do save body))
 ;;    (on (for-on elt l body))))

(defun for-in (elt l when do save splice filter)
  (if when 
      (cond (do `(mapc #'(lambda (,elt) (when ,when ,do)) ,l))
            (save `(mapcan #'(lambda (,elt) (if ,when (list ,save) nil)) ,l)))
      (cond (do `(mapc #'(lambda (,elt) ,do) ,l))
            (save `(mapcar #'(lambda (,elt) ,save) ,l))
            (filter (let ((var (gensym)))
                      `(mapcan #'(lambda (,elt) (let ((,var ,filter)) (if ,var (list ,var) nil))) ,l)))) ))


;; (for (x in '(1 2 3)) do (print x))

;; 1 
;; 2 
;; 3 
;; (1 2 3)
;; (let ((y 9)) (for (x in '(1 2 3)) do (print (+ x y))))

;; 10 
;; 11 
;; 12 
;; (1 2 3)

;; (for (x in '(1 2 3)) save (1+ x)) => (2 3 4)

;; (for (x in '(1 2 3)) filter (evenp x))
;; (T)
;; (for (x in '(a 1 2 b 3 c d 4)) filter (if (numberp x) (1+ x) nil))
;; (2 3 4 5)

;; 另见
;; (equal '(2 3 4 5) (filter #'(lambda (x) (if (numberp x) (1+ x) nil)) '(a 1 2 b 3 c d 4)))


;;;; WHEN


;; (for (x in '(1 2 3)) when (evenp x) do (print x))

;; 2 
;; (1 2 3)

;; (for (x in '(a 1 2 b 3 c d 4)) when (numberp x) save x)
;; (1 2 3 4)
;; (for (x in '(a 1 2 b 3 c d 4)) when (numberp x) save (* 2 x))
;; (2 4 6 8)


#|
;;;
;;;    Wilenksy examples 233 页
(for (x in l) (do (foo x))) => (mapc 'foo l)
(for (x on l) (do (foo x))) => (mapl 'foo l)
(for (x in l) (save (foo x))) => (mapcar 'foo l)
(for (x in l) (when (test x)) (do (foo x))) => (mapc (lambda (x) (cond ((test x) (foo x)))) l)
|#
