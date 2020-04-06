(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collect `(,var (gensym)))
     ,@body))
