;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               dbms.lisp
;;;;
;;;;   Started:            Sun Oct 25 19:42:32 2020
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :dbms (:use :common-lisp) (:shadow :delete :debug))

(in-package :dbms)

;;;
;;;    Graham's original version. (Plus `debug` function...)
;;;    All functions are closures over free variable DB, private
;;;    to the closures once MAKE-DBMS returns.
;;;    
(defun make-dbms (db)
  (list #'(lambda (key)
            (cdr (assoc key db)))
        #'(lambda (key val)
            (push (cons key val) db) ; Simply shadows existing entry if present!
            key)
        #'(lambda (key) ; Delete all!
            (setf db (cl:delete key db :key #'car))
            key)
        #'(lambda () db)))

;;;
;;;    Wrapper functions over raw list of closures.
;;;    (Dependent on order returned by MAKE-DBMS!)
;;;    
(defun select (key db) ; This parameter ordering is Graham's convention!
  (funcall (first db) key))

(defun insert (key val db)
  (funcall (second db) key val))

(defun delete (key db)
  (funcall (third db) key))

(defun debug (db)
  (funcall (fourth db)))

(defvar *db* (make-dbms '((bob . 12) (tom . 19)))) ; Don't modify literal value!
(select 'bob *db*)
(insert 'mary 20 *db*)
(dolist (key '(bob tom mary)) (print (select key *db*)))

;; (insert 'bob 99 *db*)
;; (debug *db*) => ((BOB . 99) (MARY . 20) (BOB . 12) (TOM . 19))

(delete 'bob *db*)

;; (debug *db*) => ((MARY . 20) (TOM . 19))

(dolist (key '(bob tom mary)) (print (select key *db*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :dbms-1 (:use :common-lisp) (:shadow :delete :debug))

(in-package :dbms-1)

;;;
;;;    Cleaner version with named operations.
;;;    `insert` of existing key will shadow previous entry.
;;;    
(defun make-dbms (&optional alist)
  (let ((db (copy-tree alist)))
    (labels ((insert (key val) ; Shadow!
               (push (cons key val) db)
               key)
             (select (key)
               (cdr (assoc key db)))
             (update (key val) ; Update latest entry!
               (let ((entry (assoc key db)))
                 (cond ((null entry) nil)
                       (t (setf (cdr entry) val)
                          key))))
             (delete (key) ; Delete all entries for key!
               (setf db (cl:delete key db :key #'car))
               key)
             (debug () db))
      (list #'insert #'update #'select #'delete #'debug))))

;;;
;;;    Wrapper functions still dependent on order returned by MAKE-DBMS.
;;;    
(defun insert (db key val)
  (funcall (first db) key val))

(defun update (db key val)
  (funcall (second db) key val))

(defun select (db key)
  (funcall (third db) key))

(defun delete (db key)
  (funcall (fourth db) key))

(defun debug (db)
  (funcall (fifth db)))

(defvar *db* (make-dbms '((bob . 12) (tom . 19))))
(select *db* 'bob)

;; (debug *db*) => ((BOB . 12) (TOM . 19))
;; (update *db* 'tom 22)
;; (debug *db*) => ((BOB . 12) (TOM . 22))
;; (insert *db* 'tom 23)
;; (debug *db*) => ((TOM . 23) (BOB . 12) (TOM . 22))
;; (update *db* 'tom 24)
;; (debug *db*) => ((TOM . 24) (BOB . 12) (TOM . 22))

(insert *db* 'mary 20)
(update *db* 'tom 21)
(select *db* 'tom)
(delete *db* 'mary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :dbms-2 (:use :common-lisp) (:shadow :delete :debug))

(in-package :dbms-2)

;;;
;;;    Cleaner version with named operations.
;;;    `insert` of existing key calls `update`
;;;    
(defun make-dbms (&optional alist)
  (let ((db (copy-tree alist)))
    (labels ((find-entry (key)
               (assoc key db))
             (insert (key val)
               (cond ((find-entry key) (update key val))
                     (t (push (cons key val) db)
                        key)))
             (select (key)
               (cdr (find-entry key)))
             (update (key val) ; Update latest entry!
               (let ((entry (find-entry key)))
                 (cond ((null entry) nil)
                       (t (setf (cdr entry) val)
                          key))))
             (delete (key) ; Delete all entries for key!
               (setf db (cl:delete key db :key #'car))
               key)
             (debug () db))
      (list #'insert #'update #'select #'delete #'debug))))

;;;
;;;    Wrapper functions still dependent on order returned by MAKE-DBMS.
;;;    
(defun insert (db key val)
  (funcall (first db) key val))

(defun update (db key val)
  (funcall (second db) key val))

(defun select (db key)
  (funcall (third db) key))

(defun delete (db key)
  (funcall (fourth db) key))

(defun debug (db)
  (funcall (fifth db)))

(defvar *db* (make-dbms '((bob . 12) (tom . 19))))
(select *db* 'bob)

;; (debug *db*) => ((BOB . 12) (TOM . 19))
;; (insert *db* 'tom 23)
;; (debug *db*) => ((BOB . 12) (TOM . 23))
;; (update *db* 'tom 24)
;; (debug *db*) => ((BOB . 12) (TOM . 24))

(insert *db* 'mary 20)
(update *db* 'tom 21)
(select *db* 'tom)
(delete *db* 'mary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :dbms-struct (:use :common-lisp) (:shadow :delete))

(in-package :dbms-struct)

(defstruct db insert select update delete)

;; (defun make-dbms ()
;;   (let ((data (make-hash-table :test #'equal))
;;         (db (make-db)))
;;     (labels ((insert (key val)
;;                (setf (gethash key data) val)
;;                key)
;;              (update (key val)
;;                (multiple-value-bind (old-val presentp) (gethash key data)
;;                  (cond (presentp (setf (gethash key data) val)
;;                                  old-val)
;;                        (t nil))))
;;              (select (key)
;;                (gethash key data))
;;              (delete (key)
;;                (remhash key data)))
;;       (setf (db-insert db) #'insert
;;             (db-update db) #'update
;;             (db-select db) #'select
;;             (db-delete db) #'delete))
;;     db))

(defun make-dbms ()
  (let ((data (make-hash-table :test #'equal)))
    (labels ((insert (key val)
               (setf (gethash key data) val)
               key)
             (select (key)
               (gethash key data))
             (update (key val)
               (multiple-value-bind (old-val presentp) (gethash key data)
                 (cond (presentp (setf (gethash key data) val)
                                 old-val)
                       (t nil))))
             (delete (key)
               (remhash key data)))
      (make-db :insert #'insert
               :select #'select
               :update #'update
               :delete #'delete))))

(defun insert (db key val)
  (funcall (db-insert db) key val))

(defun select (db key)
  (funcall (db-select db) key))

(defun update (db key val)
  (funcall (db-update db) key val))

(defun delete (db key)
  (funcall (db-delete db) key))

(defvar *db* (make-dbms))
(insert *db* 'bob 19)
(select *db* 'bob)
(update *db* 'bob 20)
(select *db* 'bob)
(delete *db* 'mary)
(delete *db* 'bob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :dbms-clos (:use :common-lisp) (:shadow :delete))

(in-package :dbms-clos)

(defclass db ()
  ((data :initform (make-hash-table :test #'equal))))

(defun make-dbms (&optional table)
  (let ((db (make-instance 'db)))
    (unless (null table)
      (loop for (k . v) in table
            do (insert db k v)))
    db))

;;;
;;;    `insert` of existing key overwrites entry, returns `key`
;;;    whereas proper `update` would return previous value.
;;;    
(defun insert (db key val)
  (with-slots (data) db
    (setf (gethash key data) val))
  key)

(defun select (db key)
  (with-slots (data) db
    (gethash key data)))

;;;
;;;    Cannot be used to `insert`.
;;;    
(defun update (db key val)
  (with-slots (data) db
    (multiple-value-bind (old-val presentp) (gethash key data)
      (cond (presentp (setf (gethash key data) val)
                      old-val)
            (t nil)))) )

(defun delete (db key)
  (with-slots (data) db
    (remhash key data)))


(defvar *db* (make-dbms '((bob . 12) (tom . 19))))
(select *db* 'bob)
(insert *db* 'mary 21)
(select *db* 'mary)
(update *db* 'mary 22)
(select *db* 'mary)
(delete *db* 'tom)
(select *db* 'tom)
