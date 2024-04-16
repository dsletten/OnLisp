;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               ed3.lisp
;;;;
;;;;   Started:            Sun Jan 23 17:56:31 2022
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

(defpackage :ed3 (:use :common-lisp :lang :test))

(in-package :ed3)

#!/usr/bin/env bash

EXAMPLE=$(cat <<EOF
(macrolet ((execute-lambda-at-compile-time-a (l)
             \`(quote
               ,(handler-case (funcall l)
                  (error (e) (format nil "~a" e)))))

           (execute-lambda-at-compile-time-b (l)
             \`(quote
               ,(handler-case (funcall (eval l))
                  (error (e) (format nil "~a" e)))))

           (run-example (symbol)
             \`(,symbol (lambda () 'hello))))
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
    --name example-sbcl \
    -v "${TEMP_DIR}":"/workdir" \
    -w "/workdir" \
    daewok/sbcl \
    sbcl --script ./example.lisp | grep -v "^;"
