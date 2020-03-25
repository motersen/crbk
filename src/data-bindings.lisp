(ffi:clines "#include \"src/data.h\""
            "#include <sodium.h>")

(ffi:def-function ("sodium_init" sodium-init) () :returning :int)

(defun randombytes-vec (vec)
  (ffi:c-progn (vec)
               "randombytes_vec(#0);"))

(ffi:clines "#include <string.h>"
            "#include <openssl/bio.h>")

(defun vector->b64-string (vec)
  (ffi:c-inline (vec) (:object) :object "vec_b64enc(#0)" :one-liner t))
