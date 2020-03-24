(ext:install-c-compiler)
(setq c::*speed* 3)
(setq c::*space* 3)
(setq c::*user-cc-flags* (ext:getenv "CFLAGS"))

(defvar source-file)
(defvar output-file)
(defvar unknown-option)

(defconstant +compile-rules+
  '(("-if" 1 (setq source-file 1))
    ("-of" 1 (setq output-file 1))
    ("*DEFAULT*" 1 (progn (setq unknown-option 1)
                          (format *error-output* "Unrecognized Option: ~a~%"
                                  (car unknown-option))
                          (ext:quit 1))
     :stop)))

(let ((ext:*lisp-init-file-list* nil))
  (handler-case (ext:process-command-args
                 :rules +compile-rules+
                 :args (cddr ext:*unprocessed-ecl-command-args*))
    (error (c)
      (princ ext:*help-message* *error-output*)
      (ext:quit 1))))

(compile-file source-file
              :output-file output-file
              :system-p t)
