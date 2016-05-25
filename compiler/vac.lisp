(in-package :sb-c)

(defmacro sbcl%fast-truncl (x)
  `(let* ((high (double-float-high-bits ,x))
          (low (double-float-low-bits ,x))
          (exp (ldb sb-vm:double-float-exponent-byte high))
          (biased (the double-float-exponent
                       (- exp sb-vm:double-float-bias))))
     (declare (type (signed-byte 32) high)
              (type (unsigned-byte 32) low))
     (cond
       ((= exp sb-vm:double-float-normal-exponent-max) ,x)
       ((<= biased 0) (* ,x 0d0))
       ((>= biased (float-digits ,x)) ,x)
       (t
        (let ((frac-bits (- (float-digits ,x) biased)))
          (cond ((< frac-bits 32)
                 (setf low (logandc2 low (- (ash 1 frac-bits) 1))))
                (t
                 (setf low 0)
                 (setf high (logandc2 high (- (ash 1 (- frac-bits 32)) 1)))))
          (make-double-float high low))))))

(in-package #:vacietis)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

(defmacro macroexpansion-of (form &environment env)
  `',(sb-cltl2:macroexpand-all form env))

(defun make-aref (array-var index-var &optional indexen)
  ;;(dbg "make-aref: ~S ~S~%" array-var index-var)
  (if (and (listp array-var) (eq (first array-var) 'vacietis.c:[]))
      (make-aref (second array-var) (third array-var) (append indexen (list index-var)))
      (list* 'aref array-var (nreverse (append indexen (list index-var))))))

(defmacro vac-defun/1 (name arglist &body body)
  (let ((declarations (loop for x in body
                         when (and (listp x) (eq 'declare (car x)))
                         collect x))
        (body (loop for x in body
                 when (not (and (listp x) (eq 'declare (car x))))
                 collect x)))
    (dbg "decl: ~S~%" declarations)
    (dbg "body: ~S~%" body)
    `(macrolet ((vacietis.c:truncl (x)
                  `(sb-c::sbcl%fast-truncl ,x))
                (vacietis.c:ceil (x)
                  `(ceiling ,x))
                (vacietis.c:! (a)
                  `(not ,a))
                (vacietis.c:&& (a b)
                  `(and ,a ,b))
                (vacietis.c:|\|\|| (a b)
                  `(or ,a ,b))
                (vacietis.c:< (&rest rest)
                  `(< ,@rest))
                (vacietis.c:> (&rest rest)
                  `(> ,@rest))
                (vacietis.c:<= (&rest rest)
                  `(<= ,@rest))
                (vacietis.c:>= (&rest rest)
                  `(>= ,@rest))
                (vacietis.c:+ (&rest rest)
                  `(+ ,@rest))
                (vacietis.c:- (&rest rest)
                  `(- ,@rest))
                (vacietis.c:* (&rest rest)
                  `(* ,@rest))
                (vacietis.c:/ (&rest rest)
                  `(/ ,@rest))
                (vacietis.c:[] (array-var index-var)
                  (make-aref array-var index-var))
                (vacietis.c:|.|
                  (struct-var slot-index)
                  `(aref ,struct-var ,slot-index))
                (vacietis:allocate-memory (size)
                  `(make-array ,size :element-type 'double-float))
                (vacietis.c:for ((variable-declarations
                                  initializations
                                  test
                                  step)
                                 &body body)
                  ;;(declare (ignore variable-declarations))
                  (dbg ".c:for...: ~S~%" (list variable-declarations initializations test step))
                  `(progn
                     ,initializations
                     (loop while ,test
                        do
                          ,@body
                          ,@(when step (list step))))))
       (dbg "~A ~S~%~S~%~S~%~S~%" ',name ',arglist
            ',declarations
            (macroexpansion-of ,@body)
            nil
            )
       (eval (append nil (list 'defun ',name ',arglist
                               '(declare (optimize (speed 3) (debug 0) (safety 0)))
                               ;;'(declare (optimize (speed 3)))
                               (when ',declarations ',@declarations)
                               (macroexpansion-of ,@body))))
       #+nil
       (macroexpansion-of ,@body))))
