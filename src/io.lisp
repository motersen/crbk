(ffi:clines "#include <stdio.h>"
            "#include <ecl/ecl.h>")

(defvar *stdin* (ffi:c-inline () () :pointer-void "stdin" :one-liner t))
(defvar *stdout* (ffi:c-inline () () :pointer-void "stdout" :one-liner t))
(defvar *stderr* (ffi:c-inline () () :pointer-void "stderr" :one-liner t))

(defun stdin () *stdin*)
(defun stdout () *stdout*)
(defun stderr () *stderr*)

(defun feof-p (file-pointer)
  (let ((eof (ffi:c-inline (file-pointer) (:pointer-void) :int "feof(#0)"
                           :one-liner t)))
    (not (= 0 eof))))

(defun fread (vec file-pointer &key size (count 1))
  (let ((size (or size
                  (length vec))))
    (ffi:c-inline (vec size count file-pointer)
                  (:object :int :int :pointer-void) :int
                  "fread(#0->vector.self.b8, #1, #2, #3)"
                  :one-liner t)))

(defun fread-bytes (vec file-pointer &key length)
  "Attempt to fill vec with data from file-pointer, return number of bytes read"
  (fread vec file-pointer :size 1 :count (or length
                                             (length vec))))

;; add size parameter, keep full vector as default
(defun fwrite (vec file-pointer &key size (count 1))
  (let ((size (or size
                  (length vec))))
    (ffi:c-inline (vec size count file-pointer)
                  (:object :int :int :pointer-void) :int
                 "fwrite(#0->vector.self.b8, #1, #2, #3)"
                 :one-liner t)))

(defun fflush (file-pointer)
  (ffi:c-inline (file-pointer) (:pointer-void) :int
                "fflush(#0)"
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

(defun fgets (length)
    (ffi:c-inline (length) (:int) :cstring "
char in[#0];
@(return) = fgets(in, sizeof in, stdin);"))
