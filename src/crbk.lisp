(defvar *operation* nil)
(defvar *keyfile* nil)
(defvar *ciphertext-file* nil)

(defun encrypt ()
  (let ((key (crypto-secretstream-keygen))
        (output (open *ciphertext-file*
                      :direction :output
                      :element-type '(unsigned-byte 8))))
    (encrypt-secretstream key *standard-input* output)
    (write-key-to-file *keyfile* key)
    (close output)))

(defun decrypt ()
  (let ((input (open *ciphertext-file*
                     :direction :input
                     :element-type '(unsigned-byte 8)))
        (key (read-key-from-file *keyfile*)))
    (decrypt-secretstream key input *standard-output*)
    (close input)))

(defconstant +crbk-rules+
  '((("-e" "--encrypt") 0 (setf *operation* #'encrypt))
    (("-d" "--decrypt") 0 (setf *operation* #'decrypt))
    (("-k" "--keyfile") 1 (setf *keyfile* 1))
    (("-c" "--ciphertext-file") 1 (setf *ciphertext-file* 1))))

(let ((ext:*lisp-init-file-list* nil))
  (handler-case (ext:process-command-args
                 :rules +crbk-rules+
                 :args (cdr (ext:command-args)))
    (error (c)
      (princ ext:*help-message* *error-output*)
      (ext:quit 1))))

(if (< (sodium-init) 0)
    (error "libsodium could not be initialized"))

(unless *keyfile*
  (princ "No key file specified" *error-output*)
  (terpri *error-output*)
  (ext:quit 1))
(unless *ciphertext-file*
  (princ "No ciphertext file specified" *error-output*)
  (terpri *error-output*)
  (ext:quit 1))
(unless *operation*
  (princ "No operation requested" *error-output*)
  (terpri *error-output*)
  (ext:quit 1))

(handler-bind ((size-exceeds-sequence-length-error
                #'cap-size-to-sequence-length))
  (funcall *operation*))

(ext:quit)
