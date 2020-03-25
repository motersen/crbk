(ffi:clines "#include <stdio.h>"
            "#include <ecl/ecl.h>")

(defun eof-p (file-pointer)
  (let ((eof (ffi:c-inline (file-pointer) (:pointer-void) :int "feof(#0)"
                           :one-liner t)))
    (not (= 0 eof))))

(defun fread (vec file-pointer)
  (ffi:c-inline (vec file-pointer) (:object :pointer-void) :int
                "fread(#0->vector.self.b8, 1, #0->vector.fillp, #1)"
                :one-liner t))
