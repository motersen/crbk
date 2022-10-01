(in-package #:crbk)

(defmacro assert-size-not-exceeds-sequence-length (seq size)
  (with-gensyms (sequence-expression sequence-length size-expression size-value)
    `(let ((,sequence-expression (quote ,seq))
           (,sequence-length (length ,seq))
           (,size-expression (quote ,size))
           (,size-value ,size))
       (if (<= 0 ,size-value ,sequence-length)
           ,size-value
           (restart-case (error 'size-exceeds-sequence-length-error
                                :size-expression ,size-expression
                                :size ,size-value
                                :sequence-expression ,sequence-expression
                                :sequence-length ,sequence-length)
             (cap-size-to-sequence-length () ,sequence-length))))))

(defmacro assert/length (seq size)
  `(assert-size-not-exceeds-sequence-length ,seq ,size))
