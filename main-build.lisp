;; (setq c::*delete-files* nil)

(ext:install-c-compiler)

(setq c::*speed* 3)
(setq c::*space* 3)
(setq c::*user-ld-flags* "-lsodium")
(setq c::*user-cc-flags* (ext:getenv "CFLAGS"))

(defvar object-files)
(defvar output-file)

(defconstant +compile-rules+
  '(("-of" 1 (setq output-file 1))
    ("-if" &rest (setf object-files '&rest))))

(let ((ext:*lisp-init-file-list* nil))
  (handler-case (ext:process-command-args
                 :rules +compile-rules+
                 :args (cddr ext:*unprocessed-ecl-command-args*))
    (error (c)
      (princ ext:*help-message* *error-output*)
      (ext:quit 1))))

(c:build-program output-file
                 :lisp-files object-files)
