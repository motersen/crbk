(in-package #:crbk)

(defmacro defun-c-value (lisp-name type c-name)
  `(defun ,lisp-name ()
     (ffi:c-inline () () ,type
                   ,c-name
                   :one-liner t)))
