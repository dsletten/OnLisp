;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               date.lisp
;;;;
;;;;   Started:            Wed Nov 15 18:54:09 2023
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
;;;;   Notes: SETF expander examples from Slade ch. 5
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :date (:use :common-lisp :core :test))

(in-package :date)

(defun make-date (month day year)
  (list month day year))

(defun month (date) (first date))
(defun day (date) (second date))
(defun year (date) (third date))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(defun legal-date-p (date)
  (cond ((or (> (first date) 12)
             (< (first date) 1))
         'illegal-month)
        ((or (> (second date) 31)
             (< (second date) 1)
             (and (= (first date) 2)
                  (not (leap-year-p (third date)))
                  (> (second date) 28))
             (and (= (first date) 2)
                  (leap-year-p (third date))
                  (> (second date) 29))
             (and (member (first date) '(4 6 9 11))
                  (> (second date) 30)))
         'illegal-day)
        ((not (plusp (third date))) 'illegal-year)
        (t 'ok)))



(defun legal-month-p (date)
  (<= 1 (month date) 12))

(defun legal-year-p (date)
  (plusp (year date)))

(defun legal-day-p (date)
  (let ((month (month date))
        (day (day date))
        (year (year date)))
    (cond ((< day 1) nil)
          ((= month 2) (if (leap-year-p year)
                           (<= day 29)
                           (<= day 28)))
          ((member month '(4 6 9 11)) (<= day 30))
          (t (<= day 31)))))

(defun legal-date-p (date)
  (cond ((not (legal-month-p date)) 'illegal-month)
        ((not (legal-day-p date)) 'illegal-day)
        ((not (legal-year-p date)) 'illegal-year)
        (t 'ok)))

;;;
;;;    I.
;;;    
(defun (setf month) (month date)   ; <-- Order!
  (setf (first date) month))
;  (setf (month date) month)) ; Must already be SETFable

;;;
;;;    II.
;;;
(defun set-day (date day)   ; <-- Order!
  (setf (second date) day))

(defsetf day set-day)

;;;
;;;    III.
;;;
(defsetf year (date) (year)
  `(setf (third ,date) ,year))

;; define-setf-expander
;; (get-setf-expansion '(day d))

(defpackage :clos-date (:use :common-lisp :core :test))

(in-package :clos-date)

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(defun month-length (month year)
  (ccase month
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    ((1 3 5 7 8 10 12) 31)))

;; (deftype month () '(integer 1 12)) ; Should be 0-11? See above.
;; (deftype day (m y) `(integer 1 ,(month-length m y)))
;; (deftype year () '(integer 1900 *)) ; ????

(defclass date ()
  ((month :accessor month :initarg :month :type (integer 1 12))
   (day :accessor day :initarg :day :type (integer 1 31))
   (year :accessor year :initarg :year :type (integer 0))))

(defun legal-date-p (date)
  (with-slots (year month day) date
    (check-type year (integer 0 *)) ; Allow pre-Gregorian?
    (check-type month (integer 1 12))
    (assert (typep day `(integer 1 ,(month-length month year))) (day) "Day should be between 1 and ~D." (month-length month year))))
  
(defmethod initialize-instance :after ((d date) &rest init-args)
  (declare (ignore init-args))
  (legal-date-p d))
