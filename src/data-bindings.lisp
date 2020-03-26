(ffi:clines "#include \"src/data.h\"")

(ffi:clines "#include <string.h>"
            "#include <openssl/bio.h>")

(defun vector->b64-string (vec)
  (ffi:c-inline (vec) (:object) :object "vec_b64enc(#0)" :one-liner t))
