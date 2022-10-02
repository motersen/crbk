(asdf:load-asd (make-pathname
                :directory (pathname-directory *load-pathname*)
                :name "crbk" :type "asd"))

(asdf:make :crbk)
