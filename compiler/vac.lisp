(in-package #:vacietis)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

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

(defmacro macroexpansion-of (form &environment env)
  `',(sb-cltl2:macroexpand-all form env))

(defun dimension-size (n dimensions element-size)
  (if (= n 0)
      element-size
      (* (nth (- (length dimensions) n) dimensions)
         (dimension-size (1- n) dimensions element-size))))

(defun ensure-array-type (type)
  (cond
    ((pointer-to-p type)
     (make-array-type :element-type (pointer-to-type type)))
    ((array-type-p type)
     type)
    (t (error "not an array-type: ~S~%" type))))

(defun parse-array-accessor (expr offsets &optional indexen)
  (destructuring-bind (op array-type array-var index-var)
      expr
    (declare (ignore op))
    (if (and (listp array-var) (eq (first array-var) 'vacietis.c:alien[]))
        (parse-array-accessor array-var offsets (append indexen (list index-var)))
        (let* ((array-type (ensure-array-type array-type))
               (element-type (lisp-array-element-type array-type))
               (alien-element-type (alien-type-for element-type))
               (element-size (eval `(sb-alien:alien-size ,alien-element-type :bytes)))
               (dimensions (lisp-array-dimensions array-type))
               (indices (append indexen (list index-var))))
          (dbg "  -> element-type: ~S~%" element-type)
          (parse-alien-accessor
           array-var
           (append offsets
                   (list (list* '+ (mapcar (lambda (size)
                                             `(* ,size ,(pop indices)))
                                           (loop for n from 0 upto (1- (length dimensions))
                                                collect (dimension-size n dimensions element-size)))))))))))

(defun parse-alien-accessor (expr &optional offsets)
  (dbg "parse-alien-accessor: ~S~%" expr)
  (if (atom expr)
      (values expr offsets)
      (cond
        ((eq 'vacietis.c:alien[] (car expr))
         (parse-array-accessor expr offsets))
        ((eq 'vacietis.c:|.| (car expr))
         (parse-struct-accessor expr offsets))
        (t
         (values expr offsets)))))

(defun parse-struct-accessor (expr offsets)
  (destructuring-bind (op struct-var (type slot-name))
      expr
    (declare (ignore op))
    (let* ((alien-type (or (struct-type-alien-type type)
                           (setf (struct-type-alien-type type)
                                 (eval `(sb-alien-internals:parse-alien-type ',(alien-type-for type) nil)))))
           (field (sb-alien::slot-or-lose alien-type slot-name))
           (offset (truncate (sb-alien-internals:alien-record-field-offset field) 8)))
      (parse-alien-accessor
       struct-var
       (append offsets
               (list offset))))))

(defmacro alien-slot-accessor (struct (type slot-name) &environment env)
  (dbg "alien slot accessor: ~S ~S ~S~%" struct type slot-name)
  (let* ((alien-type (or (struct-type-alien-type type)
                         (setf (struct-type-alien-type type)
                               (eval `(sb-alien-internals:parse-alien-type ',(alien-type-for type) ,env)))))
         (field (sb-alien::slot-or-lose alien-type slot-name))
         (offset (truncate (sb-alien-internals:alien-record-field-offset field) 8))
         (slot-type (nth (position slot-name (struct-type-slot-names type))
                         (struct-type-slots type)))
         (getter (sap-get-ref-for slot-type)))
    (multiple-value-bind (struct other-offsets)
        (parse-alien-accessor struct)
      (dbg "other-offsets: ~S~%" other-offsets)
      `(the ,(lisp-type-for slot-type) (,getter (c-sap-sap ,struct) ,(list* '+ offset other-offsets))))))

(defun make-alien-aref (array-type expr)
  (dbg "make-alien-aref: ~S~%" expr)
  (multiple-value-bind (array-var other-offsets)
      (parse-alien-accessor expr)
    (dbg "aref other-offsets: ~S~%" other-offsets)
    (let* ((array-type (ensure-array-type array-type))
           (element-type (lisp-array-element-type array-type))
           (getter (sap-get-ref-for element-type)))
      `(the ,(lisp-type-for element-type) (,getter (c-sap-sap ,array-var) ,(list* '+ other-offsets))))))


(defun make-aref (array-var index-var &optional indexen)
  ;;(dbg "make-aref: ~S ~S~%" array-var index-var)
  (if (and (listp array-var) (eq (first array-var) 'vacietis.c:[]))
      (make-aref (second array-var) (third array-var) (append indexen (list index-var)))
      (list* 'aref array-var (nreverse (append indexen (list index-var))))))

(defmacro vac-override (&body body)
  `(macrolet ((vacietis.c:truncl (x)
                `(sb-c::sbcl%fast-truncl ,x))
              (vacietis.c:ceil (x)
                `(ceiling ,x))
              (vacietis::do-tagbody (&body body &environment env)
                (let ((new-body
                       (let (new-body)
                         (do ((p (macroexpand body env) (cdr p)))
                             ((endp p) (nreverse new-body))
                           ;;(format t "(car p): ~S~%" (car p))
                           (cond
                             ((and (listp (car p))
                                   (eq 'vacietis.c::do (car (car p))))
                              (push 'do-loop-label new-body)
                              (dolist (x (cdr (cadr (car p))))
                                (push x new-body))
                              (let ((test (caddr (car p))))
                                (push 'do-continue new-body)
                                (push `(if ,test
                                           (go do-loop-label)
                                           (go vacietis.c:break))
                                      new-body)))
                             (t (push (car p) new-body)))))))
                  `(tagbody ,@new-body)))
              (vacietis.c:! (a)
                `(not ,a))
              (vacietis.c:!= (a b)
                `(not (eql ,a ,b)))
              (vacietis.c:== (a b)
                `(eql ,a ,b))
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
              (vacietis.c:alien[] (array-type array-var index-var)
                (make-alien-aref array-type (list 'vacietis.c:alien[] array-type array-var index-var)))
              (vacietis.c:|.|
                (struct-var slot-name-or-index)
                (if *use-alien-types*
                    `(alien-slot-accessor ,struct-var ,slot-name-or-index)
                    `(aref ,struct-var ,slot-name-or-index)))
              (vacietis.c:for ((variable-declarations
                                initialization
                                test
                                step)
                               &body body)
                ;;(dbg ".c:for...: ~S~%" (list variable-declarations initialization test step body))
                `(let* (,@variable-declarations)
                   (vacietis::do-tagbody
                       ,@(awhen initialization (list it))
                     for-loop-label
                     (when (not ,test)
                       (go vacietis.c:break))
                     ,@(when (and body (not (equal body '(nil)))) body)
                     vacietis.c:continue
                     ,@(awhen step (list it))
                     (go for-loop-label)
                     vacietis.c:break))))
     ,@body))


(defmacro vac-progn/1 (body)
  (dbg "vac-progn/1 body: ~S~%" body)
  `(vac-override
     (let ((expanded-body (macroexpansion-of ,body))
           (return-value))
       (dbg "vac-progn/1 expanded-body: ~S~%" expanded-body)
       (dbg "vac-progn/1 compiling...")
       ;;(eval expanded-body)
       (let ((compiled (compile nil `(lambda ()
                                       ,expanded-body))))
         (dbg "done.~%")
         (setq return-value (funcall compiled)))
       (dbg "vac-progn/1 returning.~%")
       return-value)))


(defmacro vac-defun/1 (name arglist &body body)
  (let ((declarations (loop for x in body
                         when (and (listp x) (eq 'declare (car x)))
                         collect x))
        (body (loop for x in body
                 when (not (and (listp x) (eq 'declare (car x))))
                 collect x)))
    (dbg "vac-defun/1 decl: ~S~%" declarations)
    (dbg "vac-defun/1 body: ~S~%" body)
    `(vac-override
       (let ((expanded-body (macroexpansion-of ,@body)))
         (dbg "vac-defun/1 name: ~A arglist: ~S~%~S~%~S~%"
              ',name ',arglist
              ',declarations
              expanded-body)
         (eval (append nil (list 'defun ',name ',arglist
                                 (list 'declare *optimize*)
                                 (when ',declarations ',@declarations)
                                 expanded-body)))))))
