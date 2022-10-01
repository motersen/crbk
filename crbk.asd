(defsystem "crbk"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "io"
                  :depends-on ("assert-size-not-exceeds-sequence-length"))
                 (:static-file "sodium-wrapper.h")
                 (:c-source-file "sodium-wrapper")
                 (:file "sodium"
                  :depends-on ("defun-c-value"
                               "assert-size-not-exceeds-sequence-length"))
                 (:file "conditions")
                 (:file "crbk")
                 (:file "defun-c-value")
                 (:file "assert-size-not-exceeds-sequence-length"
                  :depends-on ("with-gensyms"))
                 (:file "with-gensyms"))))
  :build-operation program-op
  :build-pathname "bin/crbk"
  :entry-point "crbk::main"
  :output-files (program-op
                 (o s)
                 (values
                  (list (merge-pathnames
                         (asdf::component-build-pathname s)
                         (make-pathname
                          :defaults (nth-value 2 (locate-system "crbk"))
                          :name nil :type nil)))
                  ;; final path, disable translations
                  t))
  :perform (program-op
            (o s)
            (let ((c:*user-ld-flags* "-lsodium"))
              (c:build-program (output-file o s)
			                         :lisp-files (input-files o s)
			                         :epilogue-code `(funcall (read-from-string ,(asdf::component-entry-point s))))))
  :perform (compile-bundle-op :around (o s)
                   (let ((c:*user-ld-flags* "-lsodium"))
                     (call-next-method))))

#+ecl
(defmethod output-files ((o compile-op) (c c-source-file))
  (list (make-pathname :defaults (first (input-files o c))
                       :type "o")))

#+ecl
(defmethod perform ((o compile-op) (c c-source-file))
  (ext:install-c-compiler)
  (let ((c::*user-cc-flags* (format nil "-I ~a"
                                    (namestring
                                     (component-pathname
                                      (asdf:find-component :crbk "src"))))))
    (c::compiler-cc (first (input-files o c)) (output-file o c))))

#+ecl
(defmethod perform :around ((o compile-op) (c cl-source-file))
  (let ((c:*user-cc-flags* (format nil "-I~a" (make-pathname
                                               :directory
                                               (pathname-directory
                                                (first (input-files o c)))))))
    (call-next-method)))
