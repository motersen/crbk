;; (setq c::*delete-files* nil)

(ext:install-c-compiler)

(setq c::*speed* 3)
(setq c::*space* 3)
(setq c::*user-ld-flags* "-lssl -lcrypto -lsodium")
(setq c::*user-cc-flags* (ext:getenv "CFLAGS"))

(c:build-program "bin/crbk"
                 :lisp-files '("bin/data-bindings.o"
                               "bin/data.o"
                               "bin/crbk.o"))
