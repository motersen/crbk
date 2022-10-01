(defun ensure-stream (pathname-or-stream &key direction type
                      &aux (test (ecase direction
                                   (:input #'input-stream-p)
                                   (:output #'output-stream-p))))
  (etypecase pathname-or-stream
    (stream
     (assert (funcall test pathname-or-stream))
     pathname-or-stream)
    (pathname
     (ecase direction
       (:input (assert (probe-file pathname-or-stream)))
       (:output (ensure-directories-exist pathname-or-stream)))
     (apply #'open pathname-or-stream
            `(:direction ,direction ,@(if (eq type :cipher)
                                          '(:element-type (unsigned-byte 8))))))))

(defun encrypt (keyfile &key (plain *standard-input*)
                          (cipher *standard-output*))
  "Encrypts data from PLAIN with a randomly generated key and writes it to
CIPHER. Then writes the key to the file named by filespec KEYFILE. PLAIN and
CIPHER can be streams or pathnames."
  (let ((plain-stream (etypecase plain
                     (stream
                      (assert (input-stream-p plain))
                      plain)
                     (pathname
                      (assert (probe-file plain))
                      (open plain :direction :input))))
        (cipher-stream (etypecase cipher
                      (stream
                      (assert (output-stream-p cipher))
                       cipher)
                     (pathname
                      (ensure-directories-exist cipher)
                      (open cipher :direction :output
                                :element-type '(unsigned-byte 8)))))
        (key (crypto-secretstream-keygen)))
    (unwind-protect
         (encrypt-secretstream key plain-stream cipher-stream)
      (unless (eq plain plain-stream)
        (close plain-stream))
      (unless (eq cipher cipher-stream)
        (close cipher-stream)))
    (write-key-to-file keyfile key)))

(defun decrypt (keyfile &key (cipher *standard-input*)
                          (plain *standard-output*))
  (let ((key (read-key-from-file keyfile))
        (cipher-stream (ensure-stream cipher :direction :input :type :cipher))
        (plain-stream (ensure-stream plain :direction :output :type :plain)))
    (unwind-protect
         (decrypt-secretstream key cipher-stream plain-stream)
      (unless (eq cipher cipher-stream)
        (close cipher-stream))
      (unless (eq plain plain-stream)
        (close plain-stream)))))

(defvar *operation* nil)
(defvar *key-file* nil)
(defvar *ciphertext-file* nil)
(defvar *plaintext-file* nil)

(defconstant +crbk-rules+
  '((("-e" "--encrypt") 0 (setf *operation* :encrypt))
    (("-d" "--decrypt") 0 (setf *operation* :decrypt))
    (("-k" "--key-file") 1 (setf *key-file* 1))
    (("-c" "--ciphertext-file") 1 (setf *ciphertext-file* (pathname 1)))
    (("-p" "--plaintext-file") 1 (when 1 (setf *plaintext-file* (pathname 1))))))

(defun main ()
  (let ((ext:*lisp-init-file-list* nil))
    (handler-case (ext:process-command-args
                   :rules +crbk-rules+
                   :args (cdr (ext:command-args)))
      (error (c)
        (princ ext:*help-message* *error-output*)
        (ext:quit 1))))

  (if (< (sodium-init) 0)
      (error "libsodium could not be initialized"))

  (unless *key-file*
    (princ "No key file specified" *error-output*)
    (terpri *error-output*)
    (ext:quit 1))
  (unless *operation*
    (princ "No operation requested" *error-output*)
    (terpri *error-output*)
    (ext:quit 1))

  (handler-bind ((size-exceeds-sequence-length-error
                   #'cap-size-to-sequence-length))
    (ecase *operation*
      (:encrypt
       (encrypt *key-file* :plain (or *plaintext-file* *standard-input*)
                           :cipher (or *ciphertext-file* *standard-output*)))
      (:decrypt
       (decrypt *key-file* :cipher (or *ciphertext-file* *standard-input*)
                           :plain (or *plaintext-file* *standard-output*)))))

  (ext:quit))
