(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis::*basic-c-types*
  #(int void short long float double char))

(cl:in-package #:vacietis)

(defstruct c-function
  return-type)

(defstruct c-type)

(defstruct (pointer-to (:include c-type))
  type)

(defstruct (enum-type (:include c-type))
  name)

(defstruct (struct-type (:include c-type))
  name
  slots)

(defstruct (array-type (:include c-type))
  element-type
  dimensions)

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
                                      "Array has no dimensions specified")))))

(defun unique-element-type-list (type)
  (let ((list (remove-duplicates (mapcar #'lisp-type-for (struct-type-slots type))
                                 :test #'equal)))
    (if (= 1 (length list))
        (car list)
        `(or ,@list))))
  
(defun preallocated-value-exp-for (type)
  (cond
    ((struct-type-p type)  `(make-array ,(length (struct-type-slots type))
                                        :element-type ',(unique-element-type-list type)
                                        :initial-contents (list ,@(mapcar #'preallocated-value-exp-for (struct-type-slots type)))))
    ((array-type-p type)   `(make-array ,(size-of type)
                                        :element-type ',(lisp-type-for (array-type-element-type type))))
    ((eq type 'vacietis.c:double) 0.0d0)
    ((eq type 'vacietis.c:float)  0.0e0)
    (t                     0)))

(defun %lisp-type-for-array-type (type &optional dimensions)
  (let ((element-type (array-type-element-type type))
        (type-dimensions (array-type-dimensions type)))
      (if (array-type-p element-type)
          (%lisp-type-for-array-type element-type (append dimensions type-dimensions))
          (throw :found-type (list 'simple-array (lisp-type-for element-type) (append dimensions type-dimensions))))))

(defun lisp-type-for-array-type (type &optional dimensions)
  (catch :found-type
    (%lisp-type-for-array-type type dimensions)))


;;#S(vacietis::array-type
;;                :element-type #S(vacietis::struct-type
;;                                 :name planet_pert
;;                                 :slots (#S(vacietis::array-type
;;                                            :element-type vacietis.c:double
;;                                            :dimensions (11))
;;                                         vacietis.c:double vacietis.c:double
;;                                         vacietis.c:double))
;;                :dimensions (14328))

(defun lisp-type-for (type)
  ;;(dbg "lisp-type-for type: ~S~%" type)
  (cond
    ((struct-type-p type)
     `(simple-array ,(unique-element-type-list type)
                    #+nil (or ,@(remove-duplicates (mapcar #'lisp-type-for (struct-type-slots type))
                                             :test #'equal))
                    (,(length (struct-type-slots type)))))
    ((array-type-p type)
     (lisp-type-for-array-type type))
    ((eq type 'vacietis.c:double) 'double-float)
    ((eq type 'vacietis.c:float)  'single-float)
    ((eq type 'vacietis.c:long)   '(signed-byte 64))
    ((eq type 'vacietis.c:int)    '(signed-byte 32))
    ((eq type 'vacietis.c:short)  '(signed-byte 16))
    ((eq type 'vacietis.c:char)   '(signed-byte 8))
    (t t)))

(defun lisp-type-declaration-for (type &optional name)
  (if name
      (let ((lisp-type (lisp-type-for type)))
        (unless (eq t lisp-type)
          `(type ,lisp-type ,name)))
      (when (vectorp type)
        (let* ((length (length type))
               (name (aref type (1- length))))
          (cond
            ((= length 2)
             (lisp-type-declaration-for (aref type 0) name))
            ((and (= length 3) (equalp #() (aref type 2)))
             (dbg "function pointer: ~S~%" type)
             ;; #(vacietis.c:int #(vacietis.c:* getfn) #())
             ;; XXX what about pointers to function points?
             (let ((name (aref (aref type 1) 1)))
               `(type (or function symbol integer place-ptr nil) ,name)))
            (t
             (when (eq (aref type 1) 'vacietis.c:*)
               `(type (or
                       ;;place-ptr t ;; XXX
                       (simple-array ,(lisp-type-for (aref type 0)) ,(make-list (- length 2) :initial-element '*))) ,name))))))))

(defun lisp-constant-value-for (type constant)
  (cond
    ((eq type 'vacietis.c:double) (coerce constant 'double-float))
    ((eq type 'vacietis.c:float)  (coerce constant 'single-float))
    ((eq type 'vacietis.c:long)   (the (signed-byte 64) constant))
    ((eq type 'vacietis.c:int)    (the (signed-byte 32) constant))
    ((eq type 'vacietis.c:short)  (the (signed-byte 16) constant))
    ((eq type 'vacietis.c:char)   (the (signed-byte 8)  constant))
    (t constant)))
