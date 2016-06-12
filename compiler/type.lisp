(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis::*basic-c-types*
  #(int void short long float double char))

(cl:defparameter vacietis::*unsigned-basic-c-types*
  #(unsigned-int unsigned-short unsigned-long unsigned-char))

(cl:in-package #:vacietis)

(defparameter *use-alien-types* nil)

(defstruct c-function
  inline
  return-type)

(defstruct c-type)

(defstruct (pointer-to (:include c-type))
  type)

(defstruct (enum-type (:include c-type))
  name)

(defstruct (struct-type (:include c-type))
  alien-type
  name
  slot-names
  slots)

(defstruct (array-type (:include c-type))
  alien-type
  in-struct
  element-type
  dimensions)

(defstruct c-variable
  parameter
  register
  referenced
  referenceable-name
  referenceable-type
  name
  type)

(defun type-size (type)
  (cond
    ((find type *basic-c-types*) 1)
    ((enum-type-p   type)        1)
    ((pointer-to-p  type)        1)
    ((struct-type-p type)        (reduce #'+ (map 'list #'type-size
                                                  (struct-type-slots type))))
    ((array-type-p type)         (if (array-type-dimensions type)
                                     (apply #'*
                                            (type-size
                                             (array-type-element-type type))
                                            (array-type-dimensions type))
                                     nil
                                     #+nil
                                     (error
                                      "Array has no dimensions specified"))))
  #+nil
  (eval `(sb-alien:alien-size ,(alien-type-for type))))

(defun unique-element-type-list (type)
  (let ((list (remove-duplicates (mapcar #'lisp-type-for (struct-type-slots type))
                                 :test #'equal)))
    (if (= 1 (length list))
        (car list)
        `(or ,@list))))

(defun preallocated-value-exp-for (type)
  ;;(dbg "preallocated-value-exp-for: ~S~%" type)
  (cond
    ((and *use-alien-types* (or (array-type-p type) (struct-type-p type)))
     (let ((c-pointer (gensym))
           (size (gensym))
           (alien-type (alien-type-for type)))
       `(let* ((,size (sb-alien:alien-size ,alien-type :bytes))
               (,c-pointer (array-backed-c-pointer ,size)))
          ,c-pointer)))
    ((struct-type-p type)  `(make-array ,(length (struct-type-slots type))
                                        :element-type ',(unique-element-type-list type)
                                        :initial-contents (list ,@(mapcar #'preallocated-value-exp-for (struct-type-slots type)))))
    ((array-type-p type)   `(make-array ,(or (size-of type) 1)
                                        :element-type ',(lisp-type-for (array-type-element-type type))))
    ((eq type 'vacietis.c:double) 0.0d0)
    ((eq type 'vacietis.c:float)  0.0e0)
    (t 0)))

(defun %lisp-type-for-array-type (type &optional dimensions)
  (let ((element-type (array-type-element-type type))
        (type-dimensions (array-type-dimensions type)))
    ;;(dbg "type-dimensions of ~S: ~S~%" type type-dimensions)
    (if (array-type-p element-type)
        (%lisp-type-for-array-type element-type (append dimensions type-dimensions))
        (throw :found-type (list 'simple-array (lisp-type-for element-type)
                                 (mapcar (lambda (x)
                                           (if (null x)
                                               '*
                                               x))
                                         (aif (append dimensions type-dimensions)
                                              it
                                              (list '*))))))))

(defun lisp-type-for-array-type (type &optional dimensions)
  (catch :found-type
    (%lisp-type-for-array-type type dimensions)))

(defun %alien-type-for-array-type (type &optional dimensions)
  (let ((element-type (array-type-element-type type))
        (type-dimensions (array-type-dimensions type)))
    ;;(dbg "type-dimensions of ~S: ~S~%" type type-dimensions)
    (if (array-type-p element-type)
        (%alien-type-for-array-type element-type (append dimensions type-dimensions))
        (throw :found-type (list* 'sb-alien:array (alien-type-for element-type)
                                  (aif (append dimensions type-dimensions)
                                       it
                                       (list 1)))))))

(defun alien-type-for-array-type (type &optional dimensions)
  (catch :found-type
    (%alien-type-for-array-type type dimensions)))

(defun %lisp-array-dimensions (type &optional dimensions)
  (let ((element-type (array-type-element-type type))
        (type-dimensions (array-type-dimensions type)))
    (cond
      ((pointer-to-p element-type)
       (throw :found-type (aif dimensions dimensions (list nil))))
      (t
       (if (array-type-p element-type)
           (%lisp-array-dimensions element-type (append dimensions
                                                        (aif type-dimensions
                                                             it
                                                             (list nil))))
           (throw :found-type (aif (append dimensions (aif type-dimensions
                                                             it
                                                             (list nil)))
                                   it
                                   (list nil))))))))

(defun lisp-array-dimensions (type &optional dimensions)
  "Stops at a pointer type."
  (if (pointer-to-p type)
      (list nil)
      (catch :found-type
        (%lisp-array-dimensions type dimensions))))

(defun %lisp-array-full-dimensions (type &optional dimensions)
  "Complete dimensions including pointers."
  (cond
    ((pointer-to-p type)
     (%lisp-array-full-dimensions (pointer-to-type type) (append dimensions (list nil))))
    ((array-type-p type)
     (let ((element-type (array-type-element-type type))
           (type-dimensions (array-type-dimensions type)))
       (%lisp-array-full-dimensions
        element-type (append dimensions
                             (aif type-dimensions
                                  it
                                  (list nil))))))
    (t
     (throw :found-type dimensions))))

(defun lisp-array-full-dimensions (type &optional dimensions)
  (catch :found-type
    (%lisp-array-full-dimensions type dimensions)))

(defun %lisp-array-element-type (type &optional dimensions)
  (let ((element-type
         (if (pointer-to-p type)
             (pointer-to-type type)
             (array-type-element-type type)))
        (type-dimensions (if (pointer-to-p type)
                             nil
                             (array-type-dimensions type))))
    (if (or (pointer-to-p element-type) (array-type-p element-type))
        (%lisp-array-element-type element-type (append dimensions type-dimensions))
        (throw :found-type element-type))))

(defun lisp-array-element-type (type &optional dimensions)
  (catch :found-type
    (%lisp-array-element-type type dimensions)))


;;#S(vacietis::array-type
;;                :element-type #S(vacietis::struct-type
;;                                 :name planet_pert
;;                                 :slots (#S(vacietis::array-type
;;                                            :element-type vacietis.c:double
;;                                            :dimensions (11))
;;                                         vacietis.c:double vacietis.c:double
;;                                         vacietis.c:double))
;;                :dimensions (14328))

(defun map-struct-slots (fn type)
  (loop for i from 0 upto (1- (length (struct-type-slots type)))
     collect (funcall fn
                      (nth i (struct-type-slot-names type))
                      (nth i (struct-type-slots type)))))

(defun alien-type-for (type)
  (cond
    ((struct-type-p type)
     `(sb-alien:struct
       nil
       ,@(map-struct-slots (lambda (name type)
                             (list name (alien-type-for type)))
                           type)))
    ((array-type-p type)
     (alien-type-for-array-type type))
    ((pointer-to-p type)
     `(sb-alien:* ,(alien-type-for (pointer-to-type type))))
    ((eq type 'vacietis.c:double)          'sb-alien:double)
    ((eq type 'vacietis.c:float)           'sb-alien:float)
    ((eq type 'vacietis.c:long)            'sb-alien:long)
    ((eq type 'vacietis.c:int)             'sb-alien:int)
    ((eq type 'vacietis.c:short)           'sb-alien:short)
    ((eq type 'vacietis.c:char)            'sb-alien:char)
    ((eq type 'vacietis.c:unsigned-long)   'sb-alien:unsigned-long)
    ((eq type 'vacietis.c:unsigned-int)    'sb-alien:unsigned-int)
    ((eq type 'vacietis.c:unsigned-short)  'sb-alien:unsigned-short)
    ((eq type 'vacietis.c:unsigned-char)   'sb-alien:unsigned-char)
    (t (error (format nil "unable to make alien type for ~S~%" type)))))

(defun lisp-type-for (type)
  ;;(dbg "lisp-type-for type: ~S~%" type)
  (cond
    ((and (struct-type-p type) *use-alien-types*)
     ;;'sb-sys:system-area-pointer
     'c-pointer
     #+nil
     `(sb-alien:alien ,(alien-type-for type)))
    ((and (array-type-p type) *use-alien-types*)
     ;;'sb-sys:system-area-pointer
     'c-pointer
     #+nil
     `(sb-alien:alien ,(alien-type-for type)))
    ((and (pointer-to-p type) *use-alien-types*)
     'c-pointer)
    ((struct-type-p type)
     `(simple-array ,(unique-element-type-list type)
                    (,(length (struct-type-slots type)))))
    ((array-type-p type)
     (lisp-type-for-array-type type))
    ((eq type 'vacietis.c:double)          'double-float)
    ((eq type 'vacietis.c:float)           'single-float)
    ((eq type 'vacietis.c:long)            '(signed-byte 64))
    ((eq type 'vacietis.c:int)             '(signed-byte 32))
    ((eq type 'vacietis.c:short)           '(signed-byte 16))
    ((eq type 'vacietis.c:char)            '(signed-byte 8))
    ((eq type 'vacietis.c:unsigned-long)   '(unsigned-byte 64))
    ((eq type 'vacietis.c:unsigned-int)    '(unsigned-byte 32))
    ((eq type 'vacietis.c:unsigned-short)  '(unsigned-byte 16))
    ((eq type 'vacietis.c:unsigned-char)   '(unsigned-byte 8))
    (t t)))

(defun last-symbol-in-array (array)
  (loop for i from (1- (length array)) downto 0
     when (symbolp (aref array i))
     return (aref array i)))

(defun lisp-type-declaration-for (type &optional name)
  (dbg "lisp-type-declaration-for ~S~%" type)
  (if name
      (let ((lisp-type (lisp-type-for type)))
        (unless (eq t lisp-type)
          `(type ,lisp-type ,name)))
      #+nil
      (when (vectorp type)
        (let* ((length (length type))
               (name (last-symbol-in-array type)))
          (cond
            ((= length 2)
             (lisp-type-declaration-for (aref type 0) name))
            ((and (= length 3) (equalp #() (aref type 2)))
             (dbg "function pointer: ~S~%" type)
             ;; #(vacietis.c:int #(vacietis.c:* getfn) #())
             ;; XXX what about pointers to function pointers?
             (let ((name (aref (aref type 1) 1)))
               `(type (or function symbol integer place-ptr nil) ,name)))
            (t
             (when (eq (aref type 1) 'vacietis.c:*)
               (let ((level (count-if (lambda (x)
                                        (or (eq 'vacietis.c:* x)
                                            (and (listp x)
                                                 (eq 'vacietis.c:[] (car x)))))
                                      type)))
                 (if *use-alien-types*
                     `(type c-pointer ,name)
                     ;;`(type sb-sys:system-area-pointer ,name)
                     #+nil
                     `(type (sb-alien:alien (sb-alien:array ,(alien-type-for (aref type 0)) 0)) ,name)
                     `(type
                       ;;(or
                       (simple-array ,(lisp-type-for (aref type 0)) ,(make-list level :initial-element '*))
                       ;;place-ptr t) ;; XXX
                       ,name))))))))))

(defun lisp-constant-value-for (type constant)
  (cond
    ((eq type 'vacietis.c:double)          (coerce constant 'double-float))
    ((eq type 'vacietis.c:float)           (coerce constant 'single-float))
    ((eq type 'vacietis.c:long)            (the (signed-byte 64) constant))
    ((eq type 'vacietis.c:int)             (the (signed-byte 32) constant))
    ((eq type 'vacietis.c:short)           (the (signed-byte 16) constant))
    ((eq type 'vacietis.c:char)            (the (signed-byte 8)  constant))
    ((eq type 'vacietis.c:unsigned-long)   (the (unsigned-byte 64) constant))
    ((eq type 'vacietis.c:unsigned-int)    (the (unsigned-byte 32) constant))
    ((eq type 'vacietis.c:unsigned-short)  (the (unsigned-byte 16) constant))
    ((eq type 'vacietis.c:unsigned-char)   (the (unsigned-byte 8)  constant))
    (t constant)))

(defun sap-set-ref-for (type)
  (cond
    ((eq type 'vacietis.c:double)          'sb-kernel:%set-sap-ref-double)
    ((eq type 'vacietis.c:float)           'sb-kernel:%set-sap-ref-single)
    ((eq type 'vacietis.c:long)            'sb-kernel:%set-signed-sap-ref-64)
    ((eq type 'vacietis.c:int)             'sb-kernel:%set-signed-sap-ref-32)
    ((eq type 'vacietis.c:short)           'sb-kernel:%set-signed-sap-ref-16)
    ((eq type 'vacietis.c:char)            'sb-kernel:%set-signed-sap-ref-8)
    ((eq type 'vacietis.c:unsigned-long)   'sb-kernel:%set-sap-ref-64)
    ((eq type 'vacietis.c:unsigned-int)    'sb-kernel:%set-sap-ref-32)
    ((eq type 'vacietis.c:unsigned-short)  'sb-kernel:%set-sap-ref-16)
    ((eq type 'vacietis.c:unsigned-char)   'sb-kernel:%set-sap-ref-8)
    (t nil)))

(defun sap-get-ref-for (type)
  (cond
    ((eq type 'vacietis.c:double)          'sb-sys:sap-ref-double)
    ((eq type 'vacietis.c:float)           'sb-sys:sap-ref-single)
    ((eq type 'vacietis.c:long)            'sb-sys:signed-sap-ref-64)
    ((eq type 'vacietis.c:int)             'sb-sys:signed-sap-ref-32)
    ((eq type 'vacietis.c:short)           'sb-sys:signed-sap-ref-16)
    ((eq type 'vacietis.c:char)            'sb-sys:signed-sap-ref-8)
    ((eq type 'vacietis.c:unsigned-long)   'sb-sys:sap-ref-64)
    ((eq type 'vacietis.c:unsigned-int)    'sb-sys:sap-ref-32)
    ((eq type 'vacietis.c:unsigned-short)  'sb-sys:sap-ref-16)
    ((eq type 'vacietis.c:unsigned-char)   'sb-sys:sap-ref-8)
    ((pointer-to-p type)                   'sap-ref-c-pointer)
    (t nil)))

(defun primitive-type? (type)
  (sap-get-ref-for type))
