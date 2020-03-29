(ffi:clines "#include <ecl/ecl.h>"
            "#include <sodium.h>"
            "#include \"src/sodium.h\"")

(ffi:def-function ("sodium_init" sodium-init) () :returning :int)

(ffi:def-struct :crypto-secretstream-state
    ;; wrapper for crypto_secretstream_xchacha20poly1305_state
    (k (:array :unsigned-char 32))
  (nonce (:array :unsigned-char 12))
  (pad (:array :unsigned-char 8)))

(defun crypto-secretstream-keybytes ()
  (ffi:c-inline () () :int "crypto_secretstream_xchacha20poly1305_KEYBYTES"
                :one-liner t))

(defun crypto-secretstream-abytes ()
  (ffi:c-inline () () :int "crypto_secretstream_xchacha20poly1305_ABYTES"
                :one-liner t))

(defun crypto-secretstream-headerbytes ()
  (ffi:c-inline () () :int "crypto_secretstream_xchacha20poly1305_HEADERBYTES"
                :one-liner t))

(defun crypto-secretstream-keygen ()
  (let ((key (make-array (crypto-secretstream-keybytes)
                         :element-type '(unsigned-byte 8))))
    (ffi:c-progn (key) "
crypto_secretstream_xchacha20poly1305_keygen(#0->vector.self.b8);")
    key))

(defun crypto-secretstream-init-push (state header key)
  (ffi:c-inline (state header key) (:object :object :object) :int
                "crypto_secretstream_init_push(#0, #1, #2)"
                :one-liner t))

(defun randombytes-vec (vec)
  (ffi:c-inline (vec) (:object) :void
                "randombytes_buf(#0->vector.self.b8, #0->vector.fillp);"))
