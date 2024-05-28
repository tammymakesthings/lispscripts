;; -*- Lisp; encoding: utf-8; -*-
;; From https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort …) :silent t))

(defpackage :foo
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :foo)

;;;; Configuration -----------------------------------------------
(defparameter *whatever* 123)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-foo (user-error) ()
  (:report "A foo is required, but none was supplied."))

;;;; Functionality -----------------------------------------------
(defun foo (string)
  …)

;;;; Run ---------------------------------------------------------
(defun run (arguments)
  (map nil #'foo arguments))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *ui*
  (adopt:make-interface
    :name "foo"
    …))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      … ; Handle options.
      (handler-case (run arguments)
        (user-error (e) (adopt:print-error-and-exit e))))))
