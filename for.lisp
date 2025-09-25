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
;;;;    Artificial Intelligence Programming 2e (AIP2) Charniak, Riesbeck, McDermott, Mehan §3.12-3.13
;;;;    Common LISPcraft, Robert Wilensky §13.9
;;;;
;;;;    The `for' macro consolidates (simplified versions ?? of) the various mapping functions of
;;;;    Common Lisp specified here:
;;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm
;;;;
;;;;    The 6 functions MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, MAPCON can be categorized by how
;;;;    they traverse a list and what kind of result they return.
;;;;
;;;;    3 of the functions traverse successive CARs of a list operating on the elements of
;;;;    said list: MAPC, MAPCAR, MAPCAN. These are represented by the IN operations in FOR.
;;;;
;;;;    The other 3 traverse successive CONSes, exposing each tail of the list: MAPL, MAPLIST, MAPCON.
;;;;    These are represented by ON operations in FOR.
;;;;
;;;;    Note: In fact, AIP2 does not handle the "tail" functions (MAPL, MAPLIST, MAPCON) in the FOR macro.
;;;;    This is discussed as an extension by Wilenksy.
;;;;
;;;;    Review:
;;;;    MAPC: apply function to each elt of list(s) for side effect
;;;;    (mapc #'(lambda (name) (format t "Hi ~A~%" name)) '("mjo" "Ed" "David"))  ; Unary function, single list
;;;;    Hi mjo                                                                    
;;;;    Hi Ed                                                                     
;;;;    Hi David                                                                  
;;;;    ("mjo" "Ed" "David")                                                      
;;;;                                                                              
;;;;    (mapc #'(lambda (a b) (print (list a b))) '(foo bar baz) '(:x :y :z))     ; Binary function, two lists
;;;;                                                                              
;;;;    (FOO :X)                                                                  
;;;;    (BAR :Y)                                                                  
;;;;    (BAZ :Z)                                                                  
;;;;    (FOO BAR BAZ)
;;;;
;;;;    MAPCAR: map function over elts of list(s) yielding new list as value
;;;;    (mapcar #'(lambda (n) (evenp n)) '(1 2 3 4 5))
;;;;    (NIL T NIL T NIL)
;;;;
;;;;    MAPCAN: similar to MAPCAR, but function is assumed to return list for each elt. Concatenate as single result list
;;;;    (mapcar #'(lambda (s) (list (length s) s)) '("pung" "foo" "bar"))
;;;;    ((4 "pung") (3 "foo") (3 "bar"))                                 
;;;;    (mapcan #'(lambda (s) (list (length s) s)) '("pung" "foo" "bar"))
;;;;    (4 "pung" 3 "foo" 3 "bar")                                       
;;;;
;;;;    MAPL: similar to MAPC, but iterate over each CONS in chain
;;;;    (mapc #'(lambda (elt) (print elt)) '(1 2 3))                         
;;;;                                                                         
;;;;    1                                                                    
;;;;    2                                                                    
;;;;    3                                                                    
;;;;    (1 2 3)                                                              
;;;;                                                                         
;;;;    (mapl #'(lambda (tail) (print (list (length tail) tail))) '(1 2 3))  
;;;;                                                                         
;;;;    (3 (1 2 3))                                                          
;;;;    (2 (2 3))                                                            
;;;;    (1 (3))                                                              
;;;;    (1 2 3)
;;;;
;;;;    MAPL can reproduce behavior of MAPC by reaching into CAR of each CONS
;;;;    (mapl #'(lambda (tail) (print (car tail))) '(1 2 3)) 
;;;;                                   ^^^
;;;;    1                                                    
;;;;    2                                                    
;;;;    3                                                    
;;;;    (1 2 3)                                              
;;;;
;;;;    MAPLIST: similar to MAPCAR, but yield new list from iterating over each CONS of input(s)
;;;;    (maplist #'(lambda (l) l) '(a b c d e))  
;;;;    ((A B C D E) (B C D E) (C D E) (D E) (E))
;;;;
;;;;    Likewise, MAPLIST can reproduce behavior of MAPCAR by accessing CAR of each CONS:
;;;;    (maplist #'(lambda (tail) (evenp (car tail))) '(1 2 3 4 5))
;;;;    (NIL T NIL T NIL)                                          
;;;;
;;;;    MAPCON: produces behavior of MAPCAN while iterating like MAPLIST
;;;;    (mapcon #'(lambda (l) (copy-list l)) '(a b c d e))
;;;;    (A B C D E B C D E C D E D E E)                   
;;;;
;;;;    Whoops. Don't do this! MAPCON is destructive. (NCONC)
;;;;    (mapcon #'(lambda (l) l) '(a b c d e))            
;;;;
;;;;    CLHS states the following equivalences:
;;;;    (mapcon f x1 ... xn) ≡ (apply #'nconc (maplist f x1 ... xn))
;;;;    (mapcan f x1 ... xn) ≡ (apply #'nconc (mapcar f x1 ... xn))
;;;;
;;;;    Note that MAPCON and MAPCAN are destructive due to their implicit reliance on NCONC.
;;;;
;;;;    AIP2 illustrates sample definitions for stripped down versions of the first 3 functions
;;;;    that only handle single input lists:
;;;;    (defun mapc (f l)                            
;;;;      (cond ((null l) '())                       
;;;;            (t (funcall f (first l))             
;;;;               (mapc f (rest l)))) )             
;;;;                                                 
;;;;    (defun mapcar (f l)                          
;;;;      (cond ((null l) '())                       
;;;;            (t (cons (funcall f (first l))       
;;;;                     (mapcar f (rest l)))) ))    
;;;;                                                 
;;;;    (defun mapcan (f l)                          
;;;;      (cond ((null l) '())                       
;;;;            (t (nconc (funcall f (first l))      
;;;;                      (mapcan f (rest l)))) ))   
;;;;
;;;;    In fact, their definition of MAPC is not quite right. MAPC returns the original list
;;;;    as a throwaway value. The function is used for chiefly for side effect, so the value
;;;;    is largely irrelevant:
;;;;    (defun mapc (f list)                         
;;;;      (labels ((iterate (l)                      
;;;;                 (cond ((null l) list)           
;;;;                       (t (funcall f (first l))  
;;;;                          (iterate (rest l)))) ))
;;;;        (iterate list)))                         
;;;;
;;;;    The FOR macro (at least Wilensky's version!) encompasses all 6 of these functions (as well as a cameo by REMOVE-IF-NOT).
;;;;    The macro traverses 1+ lists specified by :IN clauses.
;;;;
;;;;    Four actions may be designated:
;;;;    1. :DO Evaluate the given expression for each elt of the list (or corresponding tuples of multiple lists)
;;;;           Used for side effect. Return value is irrelevant.
;;;;    (for (x :in '(a b c)) :do (print x)) 
;;;;                                         
;;;;    A                                    
;;;;    B                                    
;;;;    C                                    
;;;;    (A B C)
;;;;
;;;;    2. :SAVE Map a list of values to a list of transformed values. Collect the result of applying the given expression
;;;;             to each elt in the list. The :SAVE expression states how to transform an individual input into the corresponding image.
;;;;    (for (x :in '(1 2 3)) :save (1+ x)) => (2 3 4)
;;;;
;;;;    3. :FILTER Evaluate the given expression for each elt. Retain the _values_ (not list elts!) for which the expression is true.
;;;;               This is Graham's FILTER function from ch. 4 (Hard to think up actual use cases!)
;;;;    (for (x :in '(a 1 2 b 3 c d 4)) :filter (and (numberp x) (1+ x))) => (2 3 4 5)
;;;;
;;;;    Graham:
;;;;    (defun filter (fn lst)                
;;;;      (let ((acc nil))                    
;;;;        (dolist (x lst)                   
;;;;          (let ((val (funcall fn x)))     
;;;;            (if val (push val acc))))     
;;;;        (nreverse acc)))                  
;;;;                                          
;;;;    or                                    
;;;;                                          
;;;;    (defun filter (f list)                
;;;;      (loop for elt in list               
;;;;            for val = (funcall f elt)     
;;;;            when val collect val))
;;;;
;;;;    4. :SPLICE Similar to :SAVE, but values for each elt are expected to be lists, which are "spliced" into final result list.
;;;;    (for (s :in '(pung foo bar baz)) :save (list s (symbol-name s))) =>
;;;;    ((PUNG "PUNG") (FOO "FOO") (BAR "BAR") (BAZ "BAZ"))
;;;;    
;;;;    (for (s :in '(pung foo bar baz)) :splice (list s (symbol-name s))) =>
;;;;    (PUNG "PUNG" FOO "FOO" BAR "BAR" BAZ "BAZ")
;;;;
;;;;    Each FOR macro may also specify a :WHEN clause to indicate a predicate that must be satisfied.
;;;;    (for (x :in '(1 2 3 4 5)) :when (evenp x) :do (print x))
;;;;                                                            
;;;;    2                                                       
;;;;    4                                                       
;;;;    (1 2 3 4 5)                                             
;;;;
;;;;    This is the conventional FILTER function:
;;;;    (for (x :in '(a 1 2 b 3 c d 4)) :when (numberp x) :save x) => (1 2 3 4)
;;;;
;;;;    Alternative ways to express Graham's FILTER above:
;;;;    (for (x :in '(a 1 2 b 3 c d 4)) :when (numberp x) :save (1+ x)) => (2 3 4 5)
;;;;    (for (x :in '(a 1 2 b 3 c d 4)) :when (numberp x) :filter (1+ x)) => (2 3 4 5)
;;;;
;;;;

#|
;;;
;;;    AIP examples 69 页
;;;
(for (x :in l) :do (foo x)) =>     (mapc #'foo l) ; Discards user-specified variable X!  Not the actual expansion?!?!
(for (x :in l) :save (foo x)) =>   (mapcar #'foo l)
(for (x :in l) :splice (foo x)) => (mapcan #'foo l)
(for (x :in l) :filter (foo x)) => (mapcan #'(lambda (x) (let ((x (foo x))) (if x (list x) nil))) l)

(for (x :in l) :when (foo x) :do (baz x)) =>     (mapc #'(lambda (x) (if (foo x) (baz x) nil)) l)
(for (x :in l) :when (foo x) :save x) =>         (remove-if-not #'foo l) ; Normal FILTER!
(for (x :in l) :when (foo x) :save (baz x)) =>   (mapcan #'(lambda (x) (if (foo x) (list (baz x)) nil)) l)
(for (x :in l) :when (foo x) :splice (baz x)) => (mapcan #'(lambda (x) (if (foo x) (baz x) nil)) l)
(for (x :in l) :when (foo x) :filter (baz x)) => (mapcan #'(lambda (x) (if (foo x) (let ((x (baz x))) (if x (list x) nil)) nil)) l)

(for (x :in l1) (y :in l2) :filter (+ x y))  Multiple vars/lists

(let ((l '(a b c d))) (for (x :in l) (y :in (rest l)) :save (list x y)))
((A B) (B C) (C D))

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

(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :for-aip2 (:use :common-lisp :core :test) (:shadow :for) (:export :for))

(in-package :for-aip2)

;;;
;;;    Clauses can appear in any order
;;;    
;;;    VARS and ARGS are extracted from 1+ :IN clauses
;;;    TEST retrieves any :WHEN expression
;;;    TYPE indicates the action to take (:DO, :SAVE, :SPLICE, :FILTER)
;;;    BODY is the expression to conduct for the TYPE.
;;;    
(defmacro for (&rest l)
  (let ((vars (for-vars l))
        (args (for-args l))
        (test (for-test l))
        (type (for-type l))
        (body (for-body l)))
    `(,(make-mapfn vars test type body)
       #',(make-lambda vars (add-test test (make-body vars test type body)))
       ,@args)))

;;;
;;;    The form produced by the expansion function takes this shape:
;;;    (<MAPPING-FN> <LAMBDA> <LIST1> [<LIST2> ...])
;;;    The work done by the expansion function and helpers is to:
;;;    1. Choose the appropriate mapping function
;;;    2. Build the correct LAMBDA
;;;          The BODY expression will be evaluated in the context of bindings
;;;          established for the VARS.
;;;    3. Extract the input list expression(s).
;;;    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Extract vars and args from :IN clauses
;;;    

;;;
;;;    A var/arg form looks like:
;;;    (<VAR> :IN <ARG>)
;;;    
(defun var-form-p (x)
  (and (consp x)
       (= (length x) 3)
       (eq (second x) :in)))

(defun var-form-var (x) (first x))
(defun var-form-arg (x) (third x))

;; (defun for-vars (l)
;;   (mapcan #'(lambda (x)
;;               (if (var-form-p x) (list (var-form-var x)) nil))
;;           l))

(defun for-vars (l)
  (loop for elt in l if (var-form-p elt) collect (var-form-var elt)))

;; (defun for-args (l)
;;   (mapcan #'(lambda (x)
;;               (if (var-form-p x) (list (var-form-arg x)) nil))
;;           l))

(defun for-args (l)
  (loop for elt in l if (var-form-p elt) collect (var-form-arg elt)))

(deftest test-for-vars ()
  (check
   (equal '(x)
          (for-vars (rest '(for (x :in '(a b c)) :do (print x)))) )
   (equal '(x y)
          (for-vars (rest '(for (x :in l) (y :in (rest l)) :save (list x y)))) )
   (equal '(x y)
          (for-vars (rest '(for (x :in '(a b c)) (y :in '(1 2 3)) :save (list x y)))) )))

(deftest test-for-args ()
  (check
   (equal '('(a b c)) ; !!!!!!!!!!
          (for-args (rest '(for (x :in '(a b c)) :do (print x)))) )
   (equal '('(a b c) '(1 2 3))
          (for-args (rest '(for (x :in '(a b c)) (y :in '(1 2 3)) :save (list x y)))) )
   (equal '(l (rest l))
          (for-aip2::for-args (rest '(for (x :in l) (y :in (rest l)) :save (list x y)))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Helper function to locate a keyword in the input expression. The value (returned
;;;    by MEMBER) on success is the tail starting with the keyword. This facilitates
;;;    retrieving the expression immediately after the keyword as in FOR-TEST/FOR-BODY.

;; (defun for-item (keywords l)
;;   (some #'(lambda (key) (member key l)) keywords))

(defun for-item (keywords l)
  (some (partial* #'member l) keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Look for a TEST clause identified by :WHEN keyword
(defun for-test (l)
  (second (for-item '(:when) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Look for TYPE and BODY value. TYPE is the keyword itself. BODY is the expression
;;;    following the TYPE.
(let ((types '(:do :save :splice :filter)))
  (defun for-type (l)
    (let ((item (for-item types l)))
      (cond ((null item) (error "No body in FOR-loop"))
            (t (first item)))) )

  (defun for-body (l)
    (let ((item (for-item types l)))
      (cond ((null item) (error "No body in FOR-loop"))
            (t (second item)))) ))

;;;
;;;    Only first `type' detected by ordering of the list above is considered.
;;;    Subsequent `type's are ignored. In other words, :DO supersedes :SAVE, which supersedes :SPLICE, etc...
;;;    (macroexpand-1 '(for (x :in l) :do (foo x)))                    <-- Identical to inputs below
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

;; (for (x :in '(1 2 3)) :save :do)
;; ; in: FOR (X :IN '(1 2 3))
;; ;     (LAMBDA (FOR-AIP2::X) NIL)
;; ; 
;; ; caught STYLE-WARNING:
;; ;   The variable X is defined but never used.
;; ; 
;; ; compilation unit finished
;; ;   caught 1 STYLE-WARNING condition
;; (1 2 3)
;; (macroexpand-1 '(for (x :in '(1 2 3)) :save :do))
;; (MAPC #'(LAMBDA (X) NIL) '(1 2 3))

;; (macroexpand-1 '(for (x :in '(1 2 3)) :save :do))
;; (MAPC #'(LAMBDA (X) NIL) '(1 2 3))

;; (for (x :in '(1 2 3)) :save :splice)
;; ; in: FOR (X :IN '(1 2 3))
;; ;     (LAMBDA (FOR-AIP2::X) :SPLICE)
;; ; 
;; ; caught STYLE-WARNING:
;; ;   The variable X is defined but never used.
;; ; 
;; ; compilation unit finished
;; ;   caught 1 STYLE-WARNING condition
;; (:SPLICE :SPLICE :SPLICE)

;; (macroexpand-1 '(for (x :in '(1 2 3)) :save :splice))
;; (MAPCAR #'(LAMBDA (X) :SPLICE) '(1 2 3))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    More appropriately called CHOOSE-MAPFN
;;;        Decides which "mapping" function to use based on contents of input.
;;;        This function returns a symbol (function designator) rather than the actual
;;;        function object as this value will be spliced in as the operator of the expansion.
;;;
(defun make-mapfn (vars test type body)
  (cond ((eq type :do) 'mapc) ; :DO implies side effect
        ((not (eq type :save)) 'mapcan) ; TYPE not :DO or :SAVE, i.e., :SPLICE or :FILTER
        ((null test) 'mapcar) ; TYPE is :SAVE
        ((use-remove-if-not-p vars body) 'remove-if-not)
        (t 'mapcan)))

(defun make-mapfn (vars test type body)
  (ecase type
    (:do 'mapc)
    ((:splice :filter) 'mapcan)
    (:save (cond ((null test) 'mapcar)
                 ((use-remove-if-not-p vars body) 'remove-if-not)
                 (t 'mapcan)))) )

(defun use-remove-if-not-p (vars body)
  (and (= (length vars) 1) (equal (first vars) body))) ; Single var and BODY is simply the variable itself.

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
