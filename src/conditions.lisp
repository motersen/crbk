(define-condition size-exceeds-sequence-length-error (error)
  ((size-expression :initarg :size-expression
                    :reader size-expression)
   (size :initarg :size
         :reader size)
   (sequence-expression :initarg :sequence-expression
                        :reader sequence-expression)
   (sequence-length :initarg :sequence-length
                    :reader sequence-length)))

(defun cap-size-to-sequence-length (c)
  (format *error-output*
          "~a exceeds length of sequence ~a: substituting ~a for ~a~%"
          (if (eq (size-expression c)
                  (size c))
              "Value"
              (format nil "Expression ~a" (size-expression c)))
          (sequence-expression c)
          (sequence-length c)
          (size c))
  (invoke-restart 'cap-size-to-sequence-length))
