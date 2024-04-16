;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               recursive.lisp
;;;;
;;;;   Started:            Wed Jun 16 12:49:01 2021
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :recursive (:use :common-lisp :lang :test))

(in-package :recursive)

(defmacro aŭ (&rest args)
  (expand args))

(defun expand (args)
  (if (null args)
      nil
      (let ((arg (gensym)))
        `(let ((,arg ,(first args)))
           (if ,arg
               ,arg
               ,(expand (rest args)))) )))

;;;
;;;    This doesn't make any sense since the macro turns over control to a function.
;;;    Args will all be evaluated now!
;;;    
;; (defun expand (args)
;;   (if (null args)
;;       nil
;;       (let ((arg (gensym)))
;;         `(let ((,arg ,(first args)))
;;            (if ,arg
;;                ,arg
;;                (expand ,@(rest args)))) ))) ; Resolves at runtime

;; (macroexpand-1 '(aŭ (> x 9) (evenp x) (= x z)))
;; (LET ((#:G8118 (> X 9)))
;;   (IF #:G8118
;;       #:G8118
;;       (LET ((#:G8119 (EVENP X)))
;;         (IF #:G8119
;;             #:G8119
;;             (LET ((#:G8120 (= X Z)))
;;               (IF #:G8120 #:G8120 NIL))))))

(defmacro ou (&rest args)
  (if (null args)
      nil
      (let ((arg (gensym)))
        `(let ((,arg ,(first args)))
           (if ,arg
               ,arg
               (ou ,@(rest args)))) )))

;; (macroexpand-1 '(ou (> x 9) (evenp x) (= x z)))
;; (LET ((#:G9515 (> X 9)))
;;   (IF #:G9515 #:G9515 (OU (EVENP X) (= X Z))))

;; (macroexpand-all '(ou (> x 9) (evenp x) (= x z)))

;; (LET ((#:G9083 (> X 9)))
;;   (IF #:G9083
;;       #:G9083
;;       (LET ((#:G9084 (EVENP X)))
;;         (IF #:G9084
;;             #:G9084
;;             (LET ((#:G9085 (= X Z)))
;;               (IF #:G9085 #:G9085 NIL))))))
