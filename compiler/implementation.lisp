(in-package #:vacietis)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

;;; unary operators

(defmacro def-unary-op (c lisp)
  (let ((x (gensym)))
   `(defmacro ,c (,x)
      `(,',lisp ,,x))))

;;; binary arithmetic

(defmacro define-binary-op-mapping-definer (name &body body)
  `(defmacro ,name (&rest map)
     `(progn ,@(loop for (vacietis cl) on map by #'cddr collect
                    (let ((op (find-symbol (symbol-name vacietis) '#:vacietis.c)))
                      ,@body)))))

(define-binary-op-mapping-definer define-binary-ops
  `(progn (declaim (inline ,op))
          (defun ,op (x y)
            (,cl x y))))

(define-binary-ops
  |\|| logior
  ^    logxor
  &    logand
  *    *
  /    /
  integer/    (lambda (x y)
                (if (and (integerp x) (integerp y))
                    (truncate x y)
                    (/ x y)))
  ptr+ %ptr+
  ptr- %ptr-
         
  %    rem
  <<   ash
  >>   (lambda (int count) (ash int (- count))))

(def-unary-op vacietis.c:~ lognot)

;;; pointers, storage units and allocation

(defstruct memptr
  mem
  (ptr 0))

(defun string-to-char* (string)
  (let ((unicode (babel:string-to-octets string :encoding :utf-8)))
    (make-array (1+ (length unicode))
                ;; XXX need to handle unsigned char
                :element-type '(signed-byte 8)
                :initial-contents (concatenate '(simple-array (signed-byte 8) (*)) unicode #(0)))))

(defun char*-to-string (char*)
  (let* ((mem        (memptr-mem char*))
         (start      (memptr-ptr char*))
         (end        (position 0 mem :start start))
         (byte-array (make-array (- end start) :element-type '(unsigned-byte 8))))
    (replace byte-array mem :start2 start :end2 end)
    (babel:octets-to-string byte-array :encoding :utf-8)))

(defun allocate-memory (size)
  (make-memptr :mem (make-array size :adjustable t :initial-element 0)))

(defstruct place-ptr
  offset
  variable
  %closure)
(defun place-ptr-closure (ptr)
  (aif (place-ptr-variable ptr)
       (aref it (place-ptr-offset ptr))
       (funcall (place-ptr-%closure ptr))))
(defun (setf place-ptr-closure) (new-value ptr)
  (aif (place-ptr-variable ptr)
       (setf (aref it (place-ptr-offset ptr)) new-value)
       (funcall (place-ptr-%closure ptr) new-value)))

(defmacro vacietis.c:mkptr& (place) ;; need to deal w/function pointers
  (let ((new-value   (gensym))
        (place       (macroexpand place)))
    (dbg "mkptr& place: ~S~%" place)
    ;; XXX assume aref/svref etc.
    (cond
      ((and (listp place)
            (member (car place) '(aref vacietis.c:[])))
       (let* ((place (copy-list place))
              (variable (second place))
              (initial-offset (third place)))
         (dbg "doing mkptr& of ~S~%" place)
         `(make-place-ptr
           :offset ,initial-offset
           :variable ,variable)))
      (t
       (dbg "attempting mkptr& of ~S~%" place)
       `(make-place-ptr
         :offset nil
         :variable nil
         :%closure (lambda (&optional ,new-value)
                     (if ,new-value
                         (setf ,place ,new-value)
                         ,place)))))))

(defun %ptr+ (ptr additional-offset)
  (let ((var (place-ptr-variable ptr))
        (offset (+ additional-offset (place-ptr-offset ptr))))
    (dbg "%ptr+ called... ~S ~S~%" ptr additional-offset)
    (make-place-ptr
     :offset offset
     :variable var)))

(defun %ptr- (ptr additional-offset)
  (%ptr+ ptr (* -1 additional-offset)))

(defun vacietis.c:deref* (ptr)
  (etypecase ptr
    (memptr    (aref (memptr-mem ptr) (memptr-ptr ptr)))
    (place-ptr (place-ptr-closure ptr))))

(defun (setf vacietis.c:deref*) (new-value ptr)
  (etypecase ptr
    (memptr    (setf (aref (memptr-mem ptr) (memptr-ptr ptr)) new-value))
    (place-ptr (setf (place-ptr-closure ptr) new-value))))

(defmacro vacietis.c:[] (a i)
  (make-aref a i)
  #+nil
  `(vacietis.c:deref* (vacietis.c:+ ,a ,i)))

(defmacro vacietis.c:|.| (x i)
  (if (and (consp x) (eq 'vacietis.c:|.| (elt x 0)))
      `(vacietis.c:|.| ,(elt x 1) ,(+ (elt x 2) i))
      `(aref ,x ,i)))

;;; arithmetic

;; things that operate on pointers: + - < > <= >= == != ++ -- !

;; may want to make these methods into cases in inlineable functions

(defmethod vacietis.c:+ ((x number) (y number))
  (+ x y))

(defmethod vacietis.c:+ ((ptr memptr) (x integer))
  (make-memptr :mem (memptr-mem ptr) :ptr (+ x (memptr-ptr ptr))))

(defmethod vacietis.c:+ ((ptr place-ptr) (x integer))
  (%ptr+ ptr x))
(defmethod vacietis.c:- ((ptr place-ptr) (x integer))
  (%ptr- ptr x))

(defmethod vacietis.c:+ ((x integer) (ptr memptr))
  (vacietis.c:+ ptr x))

(defmethod vacietis.c:- ((x number) (y number))
  (- x y))

(defmethod vacietis.c:- ((ptr memptr) (x integer))
  (make-memptr :mem (memptr-mem ptr) :ptr (- (memptr-ptr ptr) x)))

(defmethod vacietis.c:- ((ptr1 memptr) (ptr2 memptr))
  (assert (eq (memptr-mem ptr1) (memptr-mem ptr2)) ()
          "Trying to subtract pointers from two different memory segments")
  (- (memptr-ptr ptr1) (memptr-ptr ptr2)))

;;; comparison operators

(define-binary-op-mapping-definer define-comparison-ops
  `(progn (defmethod ,op ((x place-ptr) (y place-ptr))
            (and (eq  (place-ptr-variable x) (place-ptr-variable y))
                     (,cl (place-ptr-offset x) (place-ptr-offset y))))
          (defmethod ,op ((x number) (y number))
            (,cl x y))))

#+nil
(define-binary-op-mapping-definer define-comparison-ops
  `(progn (defmethod ,op ((x memptr) (y memptr))
            (if (and (eq  (memptr-mem x) (memptr-mem y))
                     (,cl (memptr-ptr x) (memptr-ptr y)))
                1
                0))
          (defmethod ,op ((x number) (y number))
            (if (,cl x y) 1 0))))

(define-comparison-ops
  == =
  <  <
  >  >
  <= <=
  >= >=)

(defmethod vacietis.c:== (x y)
  (declare (ignore x y))
  0)

;;; boolean algebra

(declaim (inline vacietis.c:!))
(defun vacietis.c:! (x)
  (if (eql x 0) 1 0))

(declaim (inline vacietis.c:!=))
(defun vacietis.c:!= (x y)
  (vacietis.c:! (vacietis.c:== x y)))

(defmacro vacietis.c:&& (a b)
  `(if (or (eql ,a 0) (eql ,b 0)) 0 1))

(defmacro vacietis.c:|\|\|| (a b)
  `(if (or (not (eql ,a 0)) (not (eql ,b 0))) 1 0))

;;; assignment

(defmacro vacietis.c:= (lvalue rvalue)
  `(setf ,lvalue ,rvalue))

(defmacro unroll-assignment-ops (&rest ops)
  `(progn
     ,@(loop for op in ops collect
            `(defmacro ,(find-symbol (symbol-name op) '#:vacietis.c)
                 (lvalue rvalue)
               `(setf ,lvalue
                      (,',(find-symbol
                           (reverse (subseq (reverse (symbol-name op)) 1))
                           '#:vacietis.c)
                          ,lvalue
                          ,rvalue))))))

(unroll-assignment-ops += -= *= integer/= ptr+= ptr-= %= <<= >>= &= ^= |\|=|)

;;; iteration

(defmacro vacietis.c:for ((bindings initialization test increment) body)
  `(let ,bindings
     (vacietis::do-tagbody ,@(awhen initialization (list it))
       for-loop-label
        (when (eql 0 ,test)
          (go vacietis.c:break))
        ,body
      vacietis.c:continue
        ,@(awhen increment (list it))
        (go for-loop-label)
      vacietis.c:break)))

(defmacro vacietis.c:do (body test)
  `(vacietis::do-tagbody do-loop-label
      ,body
    vacietis.c:continue
    (if ,test
        (go do-loop-label)
        (go vacietis.c:break))
    vacietis.c:break))

;;; switch

(defmacro vacietis.c:switch (exp cases body)
  `(vacietis::do-tagbody
      (case ,exp
        ,@(mapcar (lambda (x) `(,x (go ,x))) cases)
        (t (go ,(if (find 'vacietis.c:default body)
                    'vacietis.c:default
                    'vacietis.c:break))))
      ,@body
      vacietis.c:break))
