(ffi:clines "#include \"src/data.h\""
            "#include <sodium.h>")

(ffi:def-function ("sodium_init" sodium-init) () :returning :int)

(defun randombytes-vec (vec)
  (ffi:c-progn (vec)
               "randombytes_vec(#0);"))

(ffi:clines "#include <string.h>"
            "#include <openssl/bio.h>")

(defun vector->b64-string (vec)
  (let ((b64str))
    (ffi:c-progn (vec b64str)
                 "#1 = vec_b64enc(#0);")
    b64str))
