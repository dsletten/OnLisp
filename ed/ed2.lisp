
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ed2.lisp
;;;;
;;;;   Started:            Sun Jan 23 17:56:30 2022
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

(defpackage :ed2 (:use :common-lisp :lang :test))

(in-package :ed2)

#!/usr/bin/env bash

EXAMPLE=$(cat <<EOF
(macrolet ((execute-lambda-at-compile-time-a (l) \`(quote ,(funcall l)))
           (execute-lambda-at-compile-time-b (l) \`(quote ,(funcall (eval l))))
           (run-example (symbol)
             \`(handler-case (,symbol (lambda () 'hello))
                (error (e) (format nil "~a" e)))))
  (print (list
   (run-example funcall)
   (run-example execute-lambda-at-compile-time-b)
   (run-example execute-lambda-at-compile-time-a)
)))
EOF
)

TEMP_DIR=$(mktemp -d)

SCRIPT="${TEMP_DIR}/example.lisp"

echo "${EXAMPLE}" >> "${SCRIPT}"

echo "CLISP:"
echo
docker \
    run -it --rm \
    --name example-clisp \
    -v "${TEMP_DIR}":"/workdir" \
    -w "/workdir" \
    daewok/clisp clisp ./example.lisp

echo
echo
echo "SBCL:"
docker \
    run -it --rm \
    --name example-clisp \
    -v "${TEMP_DIR}":"/workdir" \
    -w "/workdir" \
    daewok/sbcl \
    sbcl --script ./example.lisp | grep -v "^;"
