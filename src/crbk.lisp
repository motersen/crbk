(defun fgets ()
    (ffi:c-inline () () :cstring "
char in[32];
@(return) = fgets(in, 32, stdin);"))

(let ((v (make-array 16 :element-type '(unsigned-byte 8)
                     :initial-element 0)))
  (if (< (sodium-init) 0)
      (error "libsodium could not be initialized"))
  (format t "~&~a~%~a~%" v (vector->b64-string v))
  (randombytes-vec v)
  (format t "~&~a~%~a~%" v (vector->b64-string v)))

(si:exit)
