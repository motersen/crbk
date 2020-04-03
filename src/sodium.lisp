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
    :unsigned-byte "crypto_secretstream_xchacha20poly1305_TAG_FINAL")

(defun-c-value crypto-secretstream-tag-message
    :unsigned-byte "crypto_secretstream_xchacha20poly1305_TAG_MESSAGE")

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
    (state msg ciphertext
     &key message-length (tag (crypto-secretstream-tag-message)) adata)
  (let ((message-length (or message-length
                            (length msg)))
        (alength (length adata)))
    (ffi:c-inline (state msg message-length ciphertext
                         adata alength tag)
                  (:object :object :unsigned-long-long :object
                           :object :unsigned-long-long :unsigned-byte)
                  :unsigned-long-long
                  "crypto_secretstream_push(#0, #1, #2, #3, #4, #5, #6)"
                  :one-liner t)))

(defun crypto-secretstream-init-pull (state header key)
  (ffi:c-inline (state header key) (:object :object :object) :int
                "crypto_secretstream_init_pull(#0, #1, #2)"
                :one-liner t))

(defun crypto-secretstream-pull
    (state msg ciphertext &key ciphertext-length adata)
  (let ((ciphertext-length (or ciphertext-length
                               (length ciphertext)))
        (adata-length (length adata)))
    (ffi:c-inline (state msg ciphertext ciphertext-length
                         adata adata-length)
                  (:object :object :object :unsigned-long-long
                           :object :unsigned-long-long)
                  (values :int :unsigned-byte :unsigned-long-long) "
unsigned char tag;
unsigned long long msg_length = 0;
@(return 0) = crypto_secretstream_pull(#0, #1, &msg_length, &tag, #2, #3, #4, #5);
@(return 1) = tag;
@(return 2) = msg_length;")))

(defun randombytes-vec (vec)
  (ffi:c-inline (vec) (:object) :void
                "randombytes_buf(#0->vector.self.b8, #0->vector.fillp);"))

(defun write-key-to-file (filespec key)
  (with-open-file (keyfile filespec
                           :direction :output
                           :element-type '(unsigned-byte 8))
    (fwrite key (stream-file-pointer keyfile))))

(defun read-key-from-file (filespec)
  (let ((key (make-array (crypto-secretstream-keybytes)
                         :element-type '(unsigned-byte 8))))
    (with-open-file (keyfile filespec
                             :direction :input
                             :element-type '(unsigned-byte 8))
      (fread key (stream-file-pointer keyfile))
      key)))

(defvar *chunk-size* 4096)

(macrolet ((crypto-setup (&body body)
             `(ffi:with-foreign-object (state :crypto-secretstream-state)
                (let ((ciphertext (make-array (+ *chunk-size*
                                                 (crypto-secretstream-abytes))
                                              :element-type '(unsigned-byte 8)))
                      (message (make-array *chunk-size*
                                           :element-type '(unsigned-byte 8)))
                      (header (make-array (crypto-secretstream-headerbytes)
                                          :element-type '(unsigned-byte 8)))
                      (input (stream-file-pointer input-stream))
                      (output (stream-file-pointer output-stream)))
                  ,@body))))

  (defun encrypt-secretstream (key input-stream output-stream)
    (crypto-setup
     (crypto-secretstream-init-push state header key)
     (fwrite header output)
     (labels ((encrypt-stream ()
                (let* ((message-length (fread message input
                                              :size 1 :count (length message)))
                       (eof (feof-p input))
                       (tag (if eof
                                (crypto-secretstream-tag-final)
                                (crypto-secretstream-tag-message)))
                       (ciphertext-length (crypto-secretstream-push
                                           state message ciphertext
                                           :message-length message-length
                                           :tag tag)))
                  (fwrite ciphertext output :size ciphertext-length)
                  (if (not eof)
                      (encrypt-stream)
                      (randombytes-vec message)))))
       (encrypt-stream))))

  (defun decrypt-secretstream (key input-stream output-stream)
    (crypto-setup
     (fread header input)
     (if (not (>= (crypto-secretstream-init-pull state header key) 0))
         (error "Invalid ciphertext header"))
     (labels ((decrypt-stream ()
                (let* ((ciphertext-length (fread ciphertext input
                                                 :size 1
                                                 :count (length ciphertext)))
                       (eof (feof-p input)))
                  (multiple-value-bind (exit tag message-length)
                      (crypto-secretstream-pull
                       state message ciphertext
                       :ciphertext-length ciphertext-length)
                    (if (not (= exit 0))
                        (error "Corrupted chunk"))
                    (if (and (= tag (crypto-secretstream-tag-final))
                             (not eof))
                        (error "Ciphertext ended too soon"))
                    (fwrite message output :size message-length)
                    (if (not eof)
                        (decrypt-stream)
                        (randombytes-vec message))))))
       (decrypt-stream)))))
