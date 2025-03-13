;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               if-let.lisp
;;;;
;;;;   Started:            Wed Mar 12 19:31:20 2025
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
;;;;   Notes: https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/shell")
(load "/home/slytobias/lisp/packages/time")

#|
- Read command-line args: m d y
- Validate as legal integers: month (1-12), day (1-N), year (Gregorian calendar > 1582)

|#

(defpackage :read-num (:use :common-lisp :core :io :shell :time))

(in-package :read-num)

; Read command-line arg (string) as number
(read-num "8") => 8
(read-num "8" :test #'oddp) => NIL
(read-num "asdf") => NIL
(read-num "(")
Your input is not so good: end of file on #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "(">
NIL
(read-num "(" :verbose nil) => NIL

(read-num "8.0" :test #'integralp) => 8.0d0
(read-num "8.0" :test #'integerp) => NIL

(defpackage :example1 (:use :common-lisp :core :io :shell :time))

(in-package :example1)

(defun day-of-week (d m y)
  (let* ((y₀ (- y (floor (- 14 m) 12)))
         (x (+ y₀ (floor y₀ 4) (- (floor y₀ 100)) (floor y₀ 400)))
         (m₀ (+ m (* 12 (floor (- 14 m) 12)) -2)))
    (mod (+ d x (floor (* 31 m₀) 12)) 7)))

;;
;;    Must validate M, Y before D can be checked.
;;    
(when (= (length (get-args)) 3)
  (let* ((m (read-num (get-argv 0) :test (every-pred #'integerp #'(lambda (m) (<= 1 m 12)))) )
         (y (read-num (get-argv 2) :test (every-pred #'integerp (partial #'<= 1582))))
         (d (read-num (get-argv 1) :test (every-pred #'integerp (partial* #'valid-day-p m y)))) )  ; <-- Can't evaluate if M,Y are NIL!
    (if (some #'null (list m d y))
        (format *error-output* "Corrupt~%")
        (let ((dow (day-of-week d m y)))
          (format t "~D (~A)~%" dow (get-day-of-week-name (mod (+ dow 6) 7)))) )))

;;
;;    Problems
;;    - No granularity about what went wrong...
;;    - Can't meaningfully validate D unless M and Y are valid
;;    

(defpackage :example2 (:use :common-lisp :core :io :time))

(in-package :example2)

(defun day-of-week (d m y)
  (let* ((y₀ (- y (floor (- 14 m) 12)))
         (x (+ y₀ (floor y₀ 4) (- (floor y₀ 100)) (floor y₀ 400)))
         (m₀ (+ m (* 12 (floor (- 14 m) 12)) -2)))
    (mod (+ d x (floor (* 31 m₀) 12)) 7)))

(let ((m (read-num (read-line) :test (every-pred #'integerp #'(lambda (m) (<= 1 m 12)))) ))
  (if (null m)
      (format *error-output* "Bad value for month~%")
      (let ((y (read-num (read-line) :test (every-pred #'integerp (partial #'<= 1582)))) )
        (if (null y)
            (format *error-output* "Bad value for year~%")
            (let ((d (read-num (read-line) :test (every-pred #'integerp (partial* #'valid-day-p m y)))) )
              (if (null d)
                  (format *error-output* "Bad value for day of month~%")
                  (let ((dow (day-of-week d m y)))
                    (format t "~D (~A)~%" dow (get-day-of-week-name (mod (+ dow 6) 7)))) )))) ))


(defpackage :if-let (:use :common-lisp :core :io :shell :time))

(in-package :if-let)

(defun day-of-week (d m y)
  (let* ((y₀ (- y (floor (- 14 m) 12)))
         (x (+ y₀ (floor y₀ 4) (- (floor y₀ 100)) (floor y₀ 400)))
         (m₀ (+ m (* 12 (floor (- 14 m) 12)) -2)))
    (mod (+ d x (floor (* 31 m₀) 12)) 7)))

(if-let (m (read-num (read-line) :test (every-pred #'integerp #'(lambda (m) (<= 1 m 12)))) )
  (if-let (y (read-num (read-line) :test (every-pred #'integerp (partial #'<= 1582))))
    (if-let (d (read-num (read-line) :test (every-pred #'integerp (partial* #'valid-day-p m y))))
      (let ((dow (day-of-week d m y)))
        (format t "~D (~A)~%" dow (get-day-of-week-name (mod (+ dow 6) 7))))
      (format *error-output* "Invalid day of month: ~A~%" (get-argv 1)))
    (format *error-output* "Invalid year: ~A~%" (get-argv 2)))
  (format *error-output* "Invalid month: ~A~%" (get-argv 0)))





(macroexpand-1 '(if-let (d (read-num (read-line) :test (every-pred #'integerp (partial* #'valid-day-p m y))))
      (let ((dow (day-of-week d m y)))
        (format t "~D (~A)~%" dow (get-day-of-week-name (mod (+ dow 6) 7))))
      (format *error-output* "Invalid day of month: ~A~%" (get-argv 1))))

(LET ((#:RESULT (READ-NUM (READ-LINE) :TEST (EVERY-PRED #'INTEGERP (PARTIAL* #'VALID-DAY-P M Y)))))
  (IF #:RESULT
      (LET ((D #:RESULT))
        (LET ((DOW (DAY-OF-WEEK D M Y)))
          (FORMAT T "~D (~A)~%" DOW (GET-DAY-OF-WEEK-NAME (MOD (+ DOW 6) 7)))))
      (FORMAT *ERROR-OUTPUT* "Invalid day of month: ~A~%" (GET-ARGV 1))))
