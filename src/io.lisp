(ffi:clines "#include <stdio.h>"
            "#include <ecl/ecl.h>")

(defvar *stdin* (ffi:c-inline () () :pointer-void "stdin" :one-liner t))
(defvar *stdout* (ffi:c-inline () () :pointer-void "stdout" :one-liner t))
(defvar *stderr* (ffi:c-inline () () :pointer-void "stderr" :one-liner t))

(defun stdin () *stdin*)
(defun stdout () *stdout*)
(defun stderr () *stderr*)

(defun eof-p (file-pointer)
  (let ((eof (ffi:c-inline (file-pointer) (:pointer-void) :int "feof(#0)"
                           :one-liner t)))
    (not (= 0 eof))))

;; fread+fwrite: add length parameter, keep full vector as default
(defun fread (vec file-pointer)
  (ffi:c-inline (vec file-pointer) (:object :pointer-void) :int
                "fread(#0->vector.self.b8, #0->vector.fillp, 1, #1)"
                :one-liner t))

(defun fwrite (vec file-pointer)
  (ffi:c-inline (vec file-pointer) (:object :pointer-void) :int
                "fwrite(#0->vector.self.b8, #0->vector.fillp, 1, #1)"
                :one-liner t))

(defun stream-file-pointer (stream)
  "Return FILE* pointer for stream"
  (cond ((eq stream *standard-input*)
         *stdin*)
        ((eq stream *standard-output*)
         *stdout*)
        ((eq stream *error-output*)
         *stderr*)
        (t (ffi:c-inline (stream) (:object) :pointer-void
                         "#0->stream.file.stream" :one-liner t))))
