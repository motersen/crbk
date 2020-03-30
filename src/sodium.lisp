(eval-when (:compile-toplevel)
  (let ((directory (pathname-directory *compile-file-truename*)))
    (load (make-pathname :directory directory
                         :name "defun-c-value"))))

(ffi:clines "#include <ecl/ecl.h>"
            "#include <sodium.h>"
            "#include \"src/sodium.h\"")

(ffi:def-function ("sodium_init" sodium-init) () :returning :int)

(ffi:def-struct :crypto-secretstream-state
    ;; wrapper for crypto_secretstream_xchacha20poly1305_state
    (k (:array :unsigned-char 32))
  (nonce (:array :unsigned-char 12))
  (pad (:array :unsigned-char 8)))

(defun-c-value crypto-secretstream-keybytes
    :int "crypto_secretstream_xchacha20poly1305_KEYBYTES")

(defun-c-value crypto-secretstream-abytes
    :int "crypto_secretstream_xchacha20poly1305_ABYTES")

(defun-c-value crypto-secretstream-headerbytes
    :int "crypto_secretstream_xchacha20poly1305_HEADERBYTES")

(defun-c-value crypto-secretstream-tag-final
    :unsigned-char "crypto_secretstream_xchacha20poly1305_TAG_FINAL")

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

(ffi:def-foreign-type :unsigned-long-long '(integer 0 ffi:c-ulong-long-max))

(defun crypto-secretstream-push
    (state msg ciphertext &key msglength (tag 0) adata alength)
  (let ((msglength (or msglength
                       (length msg)))
        (alength (if adata
                     (length adata)
                     0)))
    (ffi:c-inline (state msg msglength ciphertext
                         adata alength tag)
                  (:object :object :unsigned-long-long :object
                           :object :unsigned-long-long :unsigned-char)
                  :unsigned-long-long
                  "crypto_secretstream_push(#0, #1, #2, #3, #4, #5, #6)"
                  :one-liner t)))

(defun randombytes-vec (vec)
  (ffi:c-inline (vec) (:object) :void
                "randombytes_buf(#0->vector.self.b8, #0->vector.fillp);"))
