(in-package #:vacietis)

(in-readtable vacietis)

;;(declaim (optimize (debug 3)))
(declaim (optimize (speed 3) (debug 0) (safety 1)))

;;(defparameter *optimize* '(optimize (speed 0) (debug 3) (safety 3)))
;;(defparameter *optimize* '(optimize (speed 3) (debug 0) (safety 1)))
(defparameter *optimize* '(optimize (speed 3) (debug 0) (safety 0)))

(in-package #:vacietis.c)

(cl:defparameter vacietis::*type-qualifiers*
  #(inline static const signed unsigned extern auto register))

(cl:defparameter vacietis::*ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=| ? |:| |\|\|| && |\|| ^ & == != < > <= >= << >> ++ -- + - * / % ! ~ -> |.| |,|
    integer/
    truncl ceil))

(cl:defparameter vacietis::*possible-prefix-ops*
  #(! ~ sizeof - + & * ++ --))

(cl:defparameter vacietis::*ambiguous-ops*
  #(- + & *))

(cl:defparameter vacietis::*assignment-ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=|
    integer/=))

(cl:defparameter vacietis::*binary-ops-table*
  #((|\|\||)                            ; or
    (&&)                                ; and
    (|\||)                              ; logior
    (^)                                 ; logxor
    (&)                                 ; logand
    (== !=)
    (< > <= >=)
    (<< >>)                             ; ash
    (+ -)
    (* / %)))

(cl:defparameter vacietis::*math*
  #(truncl ceil))

(cl:in-package #:vacietis)

(defvar %in)
(defvar *c-file* nil)
(defvar *line-number*  nil)

(defvar *is-inline*)
(defvar *is-extern*)
(defvar *is-const*)
(defvar *is-unsigned*)

;;; a C macro can expand to several statements; READ should return all of them

(defvar *macro-stream*)

;;; error reporting

;; sbcl bug?
#+sbcl
(in-package :sb-c)
#+sbcl
(defun find-source-root (index info)
  (declare (type index index) (type source-info info))
  (let ((file-info (source-info-file-info info)))
    (handler-case
        (values (aref (file-info-forms file-info) index)
                (aref (file-info-positions file-info) index))
      (sb-int:invalid-array-index-error ()
        (format t "invalid array index: ~S  file-info: ~S~%" index file-info)))))
(in-package :vacietis)

(define-condition c-reader-error (reader-error) ;; SBCL hates simple-conditions?
  ((c-file      :reader c-file      :initform *c-file*)
   (line-number :reader line-number :initform *line-number*)
   (msg         :reader msg         :initarg  :msg))
  (:report (lambda (condition stream)
             (write-string (msg condition) stream))))

(defun read-error (msg &rest args)
  (error
   (make-condition
    'c-reader-error
    :stream %in
    :msg (format nil
                 "Error reading C stream~@[ from file ~A~]~@[ at line ~A~]:~% ~?"
                 *c-file* *line-number* msg args))))

;;; basic stream stuff

(defun c-read-char ()
  (let ((c (read-char %in nil)))
    (when (and (eql c #\Newline) *line-number*)
      (incf *line-number*))
    c))

(defun c-unread-char (c)
  (when (and (eql c #\Newline) *line-number*)
    (decf *line-number*))
  (unread-char c %in))

(defmacro loop-reading (&body body)
  `(loop with c do (setf c (c-read-char))
        ,@body))

(defun next-char (&optional (eof-error? t))
  "Returns the next character, skipping over whitespace and comments"
  (loop-reading
     while (case c
             ((nil)                     (when eof-error?
                                          (read-error "Unexpected end of file")))
             (#\/                       (%maybe-read-comment))
             ((#\Space #\Newline #\Tab) t))
     finally (return c)))

(defun make-buffer (&optional (element-type t))
  (make-array 10 :adjustable t :fill-pointer 0 :element-type element-type))

(defun slurp-while (predicate)
  (let ((string-buffer (make-buffer 'character)))
    (loop-reading
       while (and c (funcall predicate c))
       do (vector-push-extend c string-buffer)
       finally (when c (c-unread-char c)))
    string-buffer))

(defun %maybe-read-comment ()
  (case (peek-char nil %in)
    (#\/ (when *line-number* (incf *line-number*))
         (read-line %in))
    (#\* (slurp-while (let ((previous-char (code-char 0)))
                        (lambda (c)
                          (prog1 (not (and (char= previous-char #\*)
                                           (char= c #\/)))
                            (setf previous-char c)))))
         (c-read-char))))

(defun read-c-comment (%in slash)
  (declare (ignore slash))
  (%maybe-read-comment)
  (values))

;;; numbers

(defun read-octal ()
  (parse-integer (slurp-while (lambda (c) (char<= #\0 c #\7)))
                 :radix 8))

(defun read-hex ()
  (parse-integer
   (slurp-while (lambda (c)
                  (or (char<= #\0 c #\9) (char-not-greaterp #\A c #\F))))
   :radix 16))

(defun read-float (prefix separator)
  (let ((*readtable* (find-readtable :common-lisp))
        (*read-default-float-format* 'double-float))
    (read-from-string
     (format nil "~d~a~a" prefix separator
             (slurp-while (lambda (c) (find c "0123456789+-eE" :test #'char=)))))))

(defun read-decimal (c0) ;; c0 must be #\1 to #\9
  (labels ((digit-value (c) (- (char-code c) 48)))
    (let ((value (digit-value c0)))
      (loop-reading
           (cond ((null c)
                  (return value))
                 ((char<= #\0 c #\9)
                  (setf value (+ (* 10 value) (digit-value c))))
                 ((or (char-equal c #\E) (char= c #\.))
                  (return (read-float value c)))
                 (t
                  (c-unread-char c)
                  (return value)))))))

(defun read-c-number (c)
  (prog1 (if (char= c #\0)
             (let ((next (peek-char nil %in)))
               (if (digit-char-p next 8)
                   (read-octal)
                   (case next
                     ((#\X #\x) (c-read-char) (read-hex))
                     (#\.       (c-read-char) (read-float 0 #\.))
                     (otherwise 0))))
             (read-decimal c))
    (loop repeat 2 do (when (find (peek-char nil %in nil nil) "ulf" :test #'eql)
                        (c-read-char)))))

;;; string and chars (caller has to remember to discard leading #\L!!!)

(defun read-char-literal (c)
  (if (char= c #\\)
      (let ((c (c-read-char)))
        (code-char (case c
                     (#\a 7)
                     (#\f 12)
                     (#\n 10)
                     (#\r 13)
                     (#\t 9)
                     (#\v 11)
                     (#\x (read-hex))
                     (otherwise (if (char<= #\0 c #\7)
                                    (progn (c-unread-char c) (read-octal))
                                    (char-code c))))))
      c))

(defun read-character-constant (%in single-quote)
  (declare (ignore single-quote))
  (prog1 (char-code (read-char-literal (c-read-char)))
    (unless (char= (c-read-char) #\')
      (read-error "Junk in character constant"))))

(defun read-c-string (%in double-quotes)
  (declare (ignore double-quotes))
  (let ((string (make-buffer 'character)))
    (loop-reading
       (if (char= c #\") ;; c requires concatenation of adjacent string literals
           (progn (setf c (next-char nil))
                  (unless (eql c #\")
                    (when c (c-unread-char c))
                    (return `(string-to-char* ,string))))
           (vector-push-extend (read-char-literal c) string)))))

;;; preprocessor

(defvar preprocessor-if-stack ())

(defun pp-read-line ()
  (let (comment-follows?
	(escaped-newline? nil))
   (prog1
       (slurp-while (lambda (c)
		      (case c
			(#\\
			 (setf escaped-newline? t) t)
			(#\Newline escaped-newline?)
			(#\/
			 (setf escaped-newline? nil) 
			 (if (find (peek-char nil %in nil nil) "/*")
				 (progn (setf comment-follows? t) nil)
				 t))
			(t
			 (setf escaped-newline? nil) 
			 t))))
     (c-read-char)
     (when comment-follows?
       (%maybe-read-comment)))))

(defmacro lookup-define ()
  `(gethash (read-c-identifier (next-char))
            (compiler-state-pp *compiler-state*)))

(defun starts-with? (str x)
  (string= str x :end1 (min (length str) (length x))))

(defun preprocessor-skip-branch ()
  (let ((if-nest-depth 1))
    (loop for line = (pp-read-line) do
         (cond ((starts-with? line "#if")
                (incf if-nest-depth))
               ((and (starts-with? line "#endif")
                     (= 0 (decf if-nest-depth)))
                (pop preprocessor-if-stack)
                (return))
               ((and (starts-with? line "#elif")
                     (= 1 if-nest-depth))
                (case (car preprocessor-if-stack)
                  (if (when (preprocessor-test (pp-read-line))
                        (setf (car preprocessor-if-stack) 'elif)
                        (return)))
                  (elif nil)
                  (else (read-error "Misplaced #elif"))))))))

(defun preprocessor-test (line)
  (let ((exp (with-input-from-string (%in line)
               (read-infix-exp (read-c-exp (next-char))))))
    (dbg "preprocessor-test: ~S~%" exp)
    (eval `(symbol-macrolet
               ,(let ((x))
                     (maphash (lambda (k v)
                                (push (list k v) x))
                              (compiler-state-pp *compiler-state*))
                     x)
             ,exp))))

(defun fill-in-template (args template subs)
  (ppcre:regex-replace-all
   (format nil "([^a-zA-Z])?(~{~a~^|~})([^a-zA-Z0-9])?" args)
   template
   (lambda (match r1 arg r2)
     (declare (ignore match))
     (format nil "~A~A~A"
             (or r1 "")
             (elt subs (position arg args :test #'string=))
             (or r2 "")))
   :simple-calls t))

(defun c-read-delimited-strings (&optional skip-spaces?)
  (next-char) ;; skip opening paren
  (let ((paren-depth 0)
        (acc (make-buffer)))
    (with-output-to-string (sink)
      (loop for c = (c-read-char)
            until (and (= paren-depth 0) (eql #\) c)) do
            (case c
              (#\Space (unless skip-spaces? (princ c sink)))
              (#\( (incf paren-depth) (princ c sink))
              (#\) (decf paren-depth) (princ c sink))
              (#\, (vector-push-extend (get-output-stream-string sink) acc))
              (otherwise (princ c sink)))
            finally (let ((last (get-output-stream-string sink)))
                      (unless (string= last "")
                        (vector-push-extend last acc)))))
    (map 'list #'identity acc)))

(defun read-c-macro (%in sharp)
  (declare (ignore sharp))
  ;; preprocessor directives need to be read in a separate namespace
  (let ((pp-directive (read-c-identifier (next-char))))
    ;(format t "FUCKCKCKK ~a" pp-directive)
    (case pp-directive
      (vacietis.c:define
       (setf (lookup-define)
	     (let ((fuck1 (eql #\( (peek-char nil %in))))
;	       (print fuck1)
	       (if  fuck1;; no space between identifier and left paren
		   (let ((args     (c-read-delimited-strings t))
			 (template (string-trim '(#\Space #\Tab) (pp-read-line))))
;		     (print args)
		     ;;(dbg "read left paren...~%")
		     (lambda (substitutions)
		       (if args
			   (fill-in-template args template substitutions)
			   template)))
		   (pp-read-line)))))
      (vacietis.c:undef
       (remhash (read-c-identifier (next-char))
                (compiler-state-pp *compiler-state*))
       (pp-read-line))
      (vacietis.c:include
       (let* ((delimiter
               (case (next-char)
                 (#\" #\") (#\< #\>)
                 (otherwise (read-error "Error reading include path: ~A"
                                        (pp-read-line)))))
              (include-file
               (slurp-while (lambda (c) (char/= c delimiter)))))
         (next-char)
         (if (char= delimiter #\")
	     (progn
	       #+nil
	      (%load-c-file (merge-pathnames
			     include-file
			     (directory-namestring
			      (or *load-truename* *compile-file-truename*
				  *default-pathname-defaults*)))
			    *compiler-state*))
             (include-libc-file include-file))))
      (vacietis.c:if
       (push 'if preprocessor-if-stack)
       (unless (preprocessor-test (pp-read-line))
         (preprocessor-skip-branch)))
      (vacietis.c:ifdef
       (push 'if preprocessor-if-stack)
       (unless (lookup-define)
         (preprocessor-skip-branch)))
      (vacietis.c:ifndef
       (push 'if preprocessor-if-stack)
       (when (lookup-define)
         (preprocessor-skip-branch)))
      (vacietis.c:else ;; skip this branch
       (if preprocessor-if-stack
           (progn (setf (car preprocessor-if-stack) 'else)
                  (preprocessor-skip-branch))
           (read-error "Misplaced #else")))
      (vacietis.c:endif
       (if preprocessor-if-stack
           (pop preprocessor-if-stack)
           (read-error "Misplaced #endif")))
      (vacietis.c:elif
       (if preprocessor-if-stack
           (preprocessor-skip-branch)
           (read-error "Misplaced #elif")))
      (otherwise ;; line, pragma, error ignored for now
       (pp-read-line))))
  (values))

;;; types and size-of

(defun type-qualifier? (x)
  (find x *type-qualifiers*))

(defun basic-type? (x)
  (find x *basic-c-types*))

(defun unsigned-basic-type? (x)
  (find x *unsigned-basic-c-types*))

(defun c-type? (identifier)
  ;; and also do checks for struct, union, enum and typedef types
  (or (type-qualifier?        identifier)
      (basic-type?            identifier)
      (unsigned-basic-type?   identifier)
      (find identifier #(vacietis.c:struct vacietis.c:enum))
      (gethash identifier (compiler-state-typedefs *compiler-state*))))

(defvar *local-var-types* nil)
(defvar *local-variables* nil)

(defun size-of (x)
  (or (type-size x)
      (type-size (gethash x (or *local-var-types*
                                (compiler-state-var-types *compiler-state*))))))

(defun c-type-of (x)
  ;;(maphash #'(lambda (k v) (dbg "  func: ~S: ~S~%" k v)) (compiler-state-functions *compiler-state*))
  ;;(when *local-var-types* (maphash #'(lambda (k v) (dbg "  ~S: ~S~%" k v)) *local-var-types*))
  (if (and (constantp x) (numberp x))
      (typecase x
        (double-float 'vacietis.c:double)
        (single-float 'vacietis.c:float)
        (t 'vacietis.c:int))
      (or (when *local-var-types*
            (gethash x *local-var-types*))
          (gethash x (compiler-state-var-types *compiler-state*))
          (when (gethash x (compiler-state-functions *compiler-state*))
            'function))))

(defun array-type-of-exp (exp)
  (let ((type
         (if (listp exp)
             (cond
               ((eq 'vacietis.c:alien[] (car exp))
                (let ((type (array-type-of-exp (third exp))))
                  (cond ((array-type-p type)
                         type)
                        ((pointer-to-p type)
                         type)
                        (t nil))))
               ((eq 'vacietis.c:[] (car exp))
                (let ((type (array-type-of-exp (third exp))))
                  (cond ((array-type-p type)
                         type)
                        ((pointer-to-p type)
                         type)
                        (t nil))))
               (t
                (c-type-of-exp exp)))
             (c-type-of exp))))
    type))

(defun c-type-of-exp (exp &optional base-type)
  ;;(when *local-var-types* (maphash #'(lambda (k v) (dbg "  ~S: ~S~%" k v)) *local-var-types*))
  ;;(maphash #'(lambda (k v) (dbg "  ~S: ~S~%" k v)) (compiler-state-var-types *compiler-state*))
  (let ((type
         (if (listp exp)
             (cond
               ;; XXX do other ops
               ((eq 'vacietis.c:* (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'vacietis.c:/ (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'vacietis.c:+ (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'vacietis.c:- (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'vacietis.c:= (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'prog1 (car exp))
                (c-type-of-exp (cadr exp)))
               ((eq 'vacietis.c:deref* (car exp))
                (if base-type
                    (make-pointer-to :type base-type)
                    (let ((type (if *use-alien-types*
                                    (cadr exp)
                                    (c-type-of-exp (cadr exp)))))
                      (when (pointer-to-p type)
                        (pointer-to-type type)))))
               ((eq 'vacietis.c:% (car exp))
                ;; XXX assume int
                'vacietis.c:int)
               ((eq 'vacietis.c:|.| (car exp))
                (let ((struct-type (c-type-of-exp (second exp))))
                  (if *use-alien-types*
                      (nth (position (cadr (caddr exp)) (struct-type-slot-names struct-type)) (struct-type-slots struct-type))
                      (nth (caddr exp) (struct-type-slots struct-type)))))
               ((eq 'vacietis.c:[] (car exp))
                (let ((type (c-type-of-exp (second exp))))
                  (when type
                    (when (array-type-p type)
                      (array-type-element-type type)))))
               ((eq 'vacietis.c:alien[] (car exp))
                (let ((type (c-type-of-exp (third exp))))
                  (when type
                    (when (array-type-p type)
                      (array-type-element-type type)))))
               ((member (car exp) (mapcar #'(lambda (x) (intern (string-upcase x) :vacietis.c))
                                          '(&& |\|\|| < <= > >= == != ptr< ptr<= ptr> ptr>= ptr== ptr!=)))
                nil)
               ((member (car exp) '(vacietis.c::alien-ptr+ vacietis.c::alien-ptr-))
                (let ((l-c-type (c-type-of-exp (second exp)))
                      (r-c-type (c-type-of-exp (third exp))))
                  (cond
                    ((pointer-to-p l-c-type)
                     l-c-type)
                    ((pointer-to-p r-c-type)
                     r-c-type)
                    (t
                     nil))))
               ((member (car exp) '(vacietis.c::alien-ptr++ vacietis.c::alien-ptr--
                                    vacietis.c::alien-ptr+= vacietis.c::alien-ptr-=))
                (c-type-of-exp (second exp)))
               ((member (car exp) '(vacietis.c::alien-ptr< vacietis.c::alien-ptr<=
                                    vacietis.c::alien-ptr> vacietis.c::alien-ptr>=
                                    vacietis.c::alien-ptr== vacietis.c::alien-ptr!=))
                nil)
               ((member (car exp) '(cl:- cl:+ cl:* cl:/))
                (c-type-of-exp (cadr exp)))
               ((gethash (car exp) (compiler-state-functions *compiler-state*))
                (c-function-return-type (gethash (car exp) (compiler-state-functions *compiler-state*))))
               ((and (symbolp (car exp))
                     (equal "__VA_ARG" (symbol-name (car exp))))
                (make-pointer-to :type 'vacietis.c:void))
               (t
                ;; function
                ;; XXX
                'vacietis.c:int))
             (c-type-of exp))))
    (dbg "c-type-of-exp ~S: ~S~%" exp type)
    type))

(defun struct-name-of-type (type)
  (cond
    ((pointer-to-p type)
     (struct-name-of-type (pointer-to-type type)))
    ((struct-type-p type)
     (struct-type-name type))
    (t
     nil)))

;;; infix

(defvar *variable-declarations-base-type*)

(defun parse-infix (exp &optional (start 0) (end (when (vectorp exp) (length exp))) base-type)
  ;;(dbg "parse-infix: ~S ~S ~S~%" exp start end)
  (if (vectorp exp)
      (block nil
        (when (= 0 (length exp))
          (return))
        (when (= 1 (- end start))
          (return (parse-infix (aref exp start))))
        (labels ((cast? (x)
                   (and (vectorp x)
                        (not (find 'vacietis.c:|,| x)) ;;; casts can't contain commas, can they? function prototypes?
                        (some #'c-type? x)))
                 (match-binary-ops (table &key (lassoc t))
                   ;;(dbg "match-binary-ops: ~S   ~S ~S~%" table (1+ start) (1- end))
                   (let ((search-start (1+ start))
                         (search-end (1- end)))
                     (when (<= search-start search-end)
                       (position-if (lambda (x)
                                      (find x table))
                                    exp :start (1+ start) :end (1- end)
                                    :from-end lassoc))))
                 (parse-binary (i &optional op)
                   (dbg "binary-op pre parse ~S ~S-~S-~S~%" (or op (aref exp i)) start i end)
                   (let* ((lvalue (parse-infix exp start i))
                          (rvalue (parse-infix exp (1+ i) end)))
                     ;;(dbg "binary-op ~S ~S ~S~%" (or op (aref exp i)) lvalue rvalue)
                     (let ((l-c-type (if (and (not (listp lvalue)) (boundp '*variable-declarations-base-type*))
                                         *variable-declarations-base-type*
                                         (c-type-of-exp lvalue base-type)))
                           (r-c-type (c-type-of-exp rvalue))
                           (op (or op (aref exp i))))
                       (dbg "  -> type of lvalue ~S is: ~S~%" lvalue l-c-type)
                       (dbg "  -> type of rvalue ~S is: ~S~%" rvalue r-c-type)
                       (when (member op '(vacietis.c:|\|\|| vacietis.c:&&))
                         (when (integer-type? (c-type-of-exp lvalue))
                           (setq lvalue `(not (eql 0 ,lvalue))))
                         (when (integer-type? (c-type-of-exp rvalue))
                           (setq rvalue `(not (eql 0 ,rvalue)))))
                       (cond
                         ((and *use-alien-types*
                               (or (pointer-to-p l-c-type)
                                   (array-type-p l-c-type))
                               (or (not (listp lvalue))
                                   (not (eq 'vacietis.c:deref* (car lvalue)))))
                          (list
                           (case op
                             (vacietis.c:+ 'vacietis.c::alien-ptr+)
                             (vacietis.c:+= 'vacietis.c::alien-ptr+=)
                             (vacietis.c:- 'vacietis.c::alien-ptr-)
                             (vacietis.c:-= 'vacietis.c::alien-ptr-=)
                             (vacietis.c:< 'vacietis.c::alien-ptr<)
                             (vacietis.c:<= 'vacietis.c::alien-ptr<=)
                             (vacietis.c:> 'vacietis.c::alien-ptr>)
                             (vacietis.c:>= 'vacietis.c::alien-ptr>=)
                             (vacietis.c:== 'vacietis.c::alien-ptr==)
                             (vacietis.c:!= 'vacietis.c::alien-ptr!=)
                             (t op))
                           lvalue
                           (if (and (constantp rvalue) (numberp rvalue))
                               (lisp-constant-value-for l-c-type rvalue)
                               rvalue)
                           l-c-type
                           r-c-type))
                         (t
                          (list (cond
                                  ((and (or (pointer-to-p l-c-type)
                                            (array-type-p l-c-type))
                                        (or (not (listp lvalue))
                                            (not (eq 'vacietis.c:deref* (car lvalue)))))
                                   (case op
                                     (vacietis.c:+ 'vacietis.c:ptr+)
                                     (vacietis.c:+= 'vacietis.c:ptr+=)
                                     (vacietis.c:- 'vacietis.c:ptr-)
                                     (vacietis.c:-= 'vacietis.c:ptr-=)
                                     (vacietis.c:< 'vacietis.c:ptr<)
                                     (vacietis.c:<= 'vacietis.c:ptr<=)
                                     (vacietis.c:> 'vacietis.c:ptr>)
                                     (vacietis.c:>= 'vacietis.c:ptr>=)
                                     (vacietis.c:== 'vacietis.c:ptr==)
                                     (vacietis.c:!= 'vacietis.c:ptr!=)
                                     (t op)))
                                  ((pointer-to-p r-c-type)
                                   (case op
                                     (vacietis.c:+ 'vacietis.c:ptr+)
                                     (t op)))
                                  ((integer-type? l-c-type)
                                   (case op
                                     (vacietis.c:/ 'vacietis.c:integer/)
                                     (vacietis.c:/= 'vacietis.c:integer/=)
                                     (t op)))
                                  (t op))
                                lvalue
                                (if (and (constantp rvalue) (numberp rvalue))
                                    (lisp-constant-value-for l-c-type rvalue)
                                    rvalue))))))))
          ;; in order of weakest to strongest precedence
          ;; comma
          (awhen (match-binary-ops '(vacietis.c:|,|))
            (return (parse-binary it 'progn)))
          ;; assignment
          (awhen (match-binary-ops *assignment-ops* :lassoc nil)
            (return (parse-binary it)))
          ;; elvis
          (awhen (position 'vacietis.c:? exp :start start :end end)
            (let ((?pos it))
              (return
                (let* ((test (parse-infix exp start ?pos))
                       (test-type  (c-type-of-exp test))
                       (testsym (gensym)))
                  `(let ((,testsym ,test))
                     (if ,(cond
                           ((eq 'function-pointer test-type)
                            `(and ,testsym (not (eql 0 ,testsym))))
                           ((eq 'vacietis.c:int test-type)
                            `(not (eql 0 ,testsym)))
                           (t testsym))
                         ,@(aif (position 'vacietis.c:|:| exp :start ?pos :end end)
                                (list (parse-infix exp (1+ ?pos) it)
                                      (parse-infix exp (1+ it)   end))
                                (read-error "Error parsing ?: trinary operator in: ~A"
                                            (subseq exp start end)))))))))
          ;; various binary operators
          (loop for table across *binary-ops-table* do
               (awhen (match-binary-ops table)
                 ;;(dbg "matched binary op: ~S~%" it)
                 (if (and (find (elt exp it) *ambiguous-ops*)
                          (let ((prev (elt exp (1- it))))
                            (or (find prev *ops*) (cast? prev))))
                     (awhen (position-if (lambda (x)
                                           (not (or (and (find x *ops*)
                                                         (not (member x '(vacietis.c:++ vacietis.c:--))))
                                                    (cast? x))))
                                         exp
                                         :start     start
                                         :end          it
                                         :from-end      t)
                       (dbg "hmmm...ambiguous operator?~%")
                       (return-from parse-infix (parse-binary (1+ it))))
                     (return-from parse-infix (parse-binary it)))))
          ;; unary operators
          (flet ((parse-rest (i)
                   ;;(dbg "parse-rest: i: ~S  end: ~S~%" i end)
                   (parse-infix exp (1+ i) end)))
            (loop for i from start below end for x = (aref exp i) do
                 ;;(dbg "unary? ~S~%" x)
                 (cond ((cast? x)                               ;; cast
                        ;;(dbg "cast: x: ~S~%" x)
                        (return-from parse-infix (parse-rest i)))
                       ((find x #(vacietis.c:++ vacietis.c:--)) ;; inc/dec
                        ;;(dbg "inc/dec: ~S~%" x)
                        (return-from parse-infix
                          (let* ((postfix? (< start i))
                                 (place    (if postfix?
                                               (parse-infix exp start  i)
                                               (parse-infix exp (1+ i) end)))
                                 (place-type (c-type-of-exp place))
                                 ;;(__ (dbg "place-type of ~S: ~S~%" place place-type))
                                 (set-exp
                                  (cond
                                    ((and *use-alien-types*
                                          (pointer-to-p place-type)
                                          (eq x 'vacietis.c:++))
                                     `(vacietis.c::alien-ptr++ ,place ,place-type))
                                    ((and *use-alien-types*
                                          (pointer-to-p place-type)
                                          (eq x 'vacietis.c:--))
                                     `(vacietis.c::alien-ptr-- ,place ,place-type))
                                    (t
                                     `(vacietis.c:=
                                       ,place
                                       (,(if (eq x 'vacietis.c:++)
                                             (cond
                                               ((pointer-to-p place-type)
                                                'vacietis.c:ptr+)
                                               (t 'vacietis.c:+))
                                             (cond
                                               ((pointer-to-p place-type)
                                                'vacietis.c:ptr-)
                                               (t 'vacietis.c:-)))
                                         ,place
                                         1
                                         ,@(cond
                                            ((and *use-alien-types*
                                                  (pointer-to-p place-type))
                                             (list place-type (c-type-of-exp 1))))))))))
                            (if postfix?
                                `(prog1 ,place ,set-exp)
                                set-exp))))
                       ((find x *possible-prefix-ops*)          ;; prefix op
                        (dbg "prefix op: ~S  i: ~S~%" x i)
                        (return-from parse-infix
                          (if (eq x 'vacietis.c:sizeof)
                              (let ((type-exp (aref exp (1+ i))))
                                (when (vectorp type-exp) ;; fixme
                                  (setf type-exp (aref type-exp 0)))
                                (or (size-of type-exp)
                                    (read-error "Don't know sizeof ~A" type-exp)))
                              (let ((rest (parse-rest i)))
                                (cond
                                  ((eq x 'vacietis.c:!)
                                   (if (integer-type? (c-type-of-exp rest))
                                       `(if (eql 0 ,rest)
                                            1
                                            0)
                                       `(vacietis.c:! ,rest)))
                                  ((eq x 'vacietis.c:&)
                                   (let ((type (c-type-of-exp rest)))
                                     (dbg "rest type: ~S~%" type)
                                     (list 'vacietis.c:mkptr& rest)))
                                  (t
                                   (dbg "  -> x: ~S rest: ~S~%" x rest)
                                   (list* (case x
                                            (vacietis.c:- '-)
                                            (vacietis.c:* 'vacietis.c:deref*)
                                            (vacietis.c:& 'vacietis.c:mkptr&)
                                            (otherwise     x))
                                          (append
                                           (when (and *use-alien-types*
                                                      (eq x 'vacietis.c:*))
                                             (list (c-type-of-exp rest)))
                                           (list rest))))))))))))
          ;; funcall, aref, and struct access
          (loop for i from (1- end) downto (1+ start) for x = (aref exp i) do
               (cond
                 ((find x #(vacietis.c:|.| vacietis.c:->))
                  (let ((exp (parse-binary i)))
                    (let ((ctype (c-type-of-exp (elt exp 1))))
                      (dbg "struct accessor: ctype of ~S: ~S~%" (elt exp 1) ctype)
                      (return-from parse-infix
                        `(vacietis.c:|.|
                                     ,(if (eq x 'vacietis.c:->)
                                          `(vacietis.c:deref* ,(elt exp 1))
                                          (elt exp 1))
                                     ,(if *use-alien-types*
                                          (list ctype
                                                (gethash (format nil "~A.~A" (struct-name-of-type ctype) (elt exp 2))
                                                         (compiler-state-accessors *compiler-state*)))
                                          (gethash (format nil "~A.~A" (struct-name-of-type ctype) (elt exp 2))
                                                   (compiler-state-accessors *compiler-state*))))))))
                 ((listp x) ;; aref
                  (return-from parse-infix
                    (if (eq (car x) 'vacietis.c:[])
                        (let* ((array (parse-infix exp start i))
                               (index (parse-infix (second x)))
                               (array-type (array-type-of-exp array)))
                          (dbg "array-type of ~S: ~S~%" array array-type)
                          (if *use-alien-types*
                              `(vacietis.c:alien[] ,@(when array-type (list array-type)) ,array ,index)
                              `(vacietis.c:[] ,array ,index)))
                        (read-error "Unexpected list when parsing ~A" exp))))
                 ((vectorp x) ;; funcall
                  (dbg "funcall: ~S~%" x)
                  (return-from parse-infix
                    (let ((fun-exp (parse-infix exp start i)))
                     (append
                      (cond
                        ((symbolp fun-exp)
                         (list fun-exp))
                        ((and (listp fun-exp)
                              (eql 'fdefinition (car fun-exp)))
                         (list (cadr (cadr fun-exp))))
                        (t
                         (list 'funcall fun-exp)))
                      (loop with xstart = 0
                            for next = (position 'vacietis.c:|,| x :start xstart)
                            when (< 0 (length x))
                              collect (parse-infix x xstart (or next (length x)))
			 while next do (setf xstart (1+ next)))))))))
	  (return-from parse-infix "nope")
	  #+nil
          (read-error "Error parsing expression: ~A"
		      (progn (print exp)
			     (print start)
			     (print end)
			     (subseq exp start end)))))
      (progn
        (let ((type (c-type-of-exp exp)))
          ;;(dbg "type of ~S: ~S~%" exp type)
          (case type
            ('function
             `(fdefinition ',exp))
            (t exp))))))

;;; statements

(defun read-c-block (c)
  (if (eql c #\{)
      (loop for c = (next-char)
            until (eql c #\}) append (reverse
                                      (multiple-value-list
                                       (read-c-statement c))))
      (read-error "Expected opening brace '{' but found '~A'" c)))

(defun next-exp ()
  (read-c-exp (next-char)))

(defvar *variable-lisp-type-declarations*)
(defvar *variable-declarations*)
(defvar *cases*)

(defun read-exps-until (predicate)
  (let ((exps (make-buffer)))
    (loop for c = (next-char)
          until (funcall predicate c)
       do (progn
            ;;(dbg "read c: ~S~%" c)
            (vector-push-extend (read-c-exp c) exps)))
    exps))

(defun c-read-delimited-list (open-delimiter separator)
  (let ((close-delimiter (ecase open-delimiter (#\( #\)) (#\{ #\}) (#\; #\;)))
        (list            (make-buffer))
        done?)
    (loop until done? do
         (vector-push-extend
          (read-exps-until (lambda (c)
                             (cond ((eql c close-delimiter) (setf done? t))
                                   ((eql c separator)       t))))
          list))
    list))

(defun integer-type? (type)
  (member type
          '(vacietis.c:long vacietis.c:int vacietis.c:short vacietis.c:char
            vacietis.c:unsigned-long vacietis.c:unsigned-int vacietis.c:unsigned-short vacietis.c:unsigned-char)))

(defvar *function-name*)

(defun copy-hash-table (hash-table)
  (let ((copy (make-hash-table
               :test (hash-table-test hash-table)
               :rehash-size (hash-table-rehash-size hash-table)
               :rehash-threshold (hash-table-rehash-threshold hash-table)
               :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key copy) value))
    copy))

(defun read-control-flow-statement (statement)
  (flet ((read-block-or-statement ()
           (let ((next-char (next-char)))
             (if (eql next-char #\{)
                 ;;#+nil ;; XXX check if tagbody exists
                 (cons 'tagbody (read-c-block next-char))
                 (read-c-statement next-char)))))
    (if (eq statement 'vacietis.c:if)
        (let* ((test       (parse-infix (next-exp)))
               (test-type  (c-type-of-exp test))
               (then       (read-block-or-statement))
               (next-char  (next-char nil))
               (next-token (case next-char
                             (#\e  (read-c-exp #\e))
                             ((nil))
                             (t    (c-unread-char next-char) nil)))
               (if-exp    (cond
                            ((eq 'function test-type)
                             (let ((testsym (gensym)))
                               `(let ((,testsym ,test))
                                  (if (and ,testsym (not (eql 0 ,testsym)))
                                      ,then
                                      ,(when (eq next-token 'vacietis.c:else)
                                             (read-block-or-statement))))))
                            ((integer-type? test-type)
                             `(if (not (eql 0 ,test))
                                    ,then
                                    ,(when (eq next-token 'vacietis.c:else)
                                           (read-block-or-statement))))
                            (t `(if ,test
                                    ,then
                                    ,(when (eq next-token 'vacietis.c:else)
                                           (read-block-or-statement))))))
               #+nil
               (if-exp    `(if (eql 0 ,test)
                               ,(when (eq next-token 'vacietis.c:else)
                                      (read-block-or-statement))
                               ,then)))
          ;;(dbg "if-exp: ~S~%" if-exp)
          (if (or (not next-token) (eq next-token 'vacietis.c:else))
              if-exp
              `(progn ,if-exp ,(%read-c-statement next-token))))
        (case statement
          ((vacietis.c:break vacietis.c:continue)
            `(go ,statement))
          (vacietis.c:goto
            `(go ,(read-c-statement (next-char))))
          (vacietis.c:return
            `(return-from ,*function-name* ,(or (read-c-statement (next-char)) 0)))
          (vacietis.c:case
            (prog1 (car (push
                         (eval (parse-infix (next-exp))) ;; must be constant int
                         *cases*))
              (unless (eql #\: (next-char))
                (read-error "Error parsing case statement"))))
          (vacietis.c:switch
            (let* ((exp     (parse-infix (next-exp)))
                   (*cases* ())
                   (body    (read-c-block (next-char))))
              `(vacietis.c:switch ,exp ,*cases* ,body)))
          (vacietis.c:while
           (let ((test (parse-infix (next-exp))))
             (dbg "while test type: ~S~%" (c-type-of-exp test))
             (cond
               ((integer-type? (c-type-of-exp test))
                `(vacietis.c:for (nil nil (not (eql 0 ,test)) nil)
                                 ,(read-block-or-statement)))
               (t
                `(vacietis.c:for (nil nil ,test nil)
                                 ,(read-block-or-statement)))))
           #+nil
           `(vacietis.c:for (nil nil ,(parse-infix (next-exp)) nil)
                            ,(read-block-or-statement)))
          (vacietis.c:do
            (let ((body (read-block-or-statement)))
              (if (eql (next-exp) 'vacietis.c:while)
                  (let ((test (parse-infix (next-exp))))
                    (prog1 `(vacietis.c:do ,body ,(if (integer-type? (c-type-of-exp test)) `(not (eql 0 ,test)) test))
                      (read-c-statement (next-char)))) ;; semicolon
                  (read-error "No 'while' following a 'do'"))))
          (vacietis.c:for
            `(vacietis.c:for
              ,(let* ((*local-var-types* (if *local-var-types*
                                             (copy-hash-table *local-var-types*)
                                             (make-hash-table)))
                      (*variable-declarations* ()) ;; c99, I think?
                      (*variable-lisp-type-declarations* ())
                      (initializations (progn
                                         (next-char)
                                         (read-c-statement
                                          (next-char)))))
                     (list* *variable-declarations*
                            initializations
                            (map 'list
                                 #'parse-infix
                                 (c-read-delimited-list #\( #\;))))
              ,(read-block-or-statement)))))))

(defun read-function (name result-type)
  (let (arglist
        arglist-type-declarations
        local-arglist-declarations
        local-arglist-lisp-type-declarations
        (*function-name* name)
        (*local-var-types* (make-hash-table))
        (*local-variables* (make-hash-table))

	(parameters (c-read-delimited-list (next-char) #\,)))
    (block done-arglist
      (loop for param across parameters do
           (block done-arg
             (let ((ptrlev 0)
                   (arg-name)
                   (arg-type
                    (when (and (vectorp param) (c-type? (aref param 0)))
                      (aref param 0))))
               ;; XXX
               (when (eq 'vacietis.c:const arg-type)
                 (when (and (vectorp param) (c-type? (aref param 1)))
                   (setq arg-type (aref param 1))))
               (dbg "param: arg-type: ~S~%" arg-type)
               (labels ((strip-type (x)
                          (cond ((symbolp x)
                                 (when (> ptrlev 0)
                                   (let ((type arg-type))
                                     (loop while (> ptrlev 0)
                                        do (setq type (make-pointer-to :type type))
                                          (decf ptrlev))
                                     (setq arg-type type)))
                                 (setq arg-name x)
                                 (setf (gethash arg-name *local-var-types*) arg-type)
                                 (setf (gethash arg-name *local-variables*)
                                       (make-c-variable :name arg-name
                                                        :parameter t
                                                        :type arg-type))
                                 (when *use-alien-types*
                                   (cond
                                     ((pointer-to-p arg-type)
                                      (let ((passed-arg-name (intern (format nil "ARG-~A" (symbol-name arg-name))
                                                                     #+nil
                                                                     (symbol-package arg-name))))
                                        (push `(,arg-name (vacietis::copy-c-pointer ,passed-arg-name))
                                              local-arglist-declarations)
                                        (push `(dynamic-extent ,arg-name)
                                              local-arglist-lisp-type-declarations)
                                        (push `(type vacietis::c-pointer ,arg-name)
                                              local-arglist-lisp-type-declarations)
                                        (setq arg-name passed-arg-name)))))
                                 (push arg-name arglist))
                                ((vectorp x)
                                 (loop for x1 across x do
                                      (when (not (or (c-type? x1)
                                                     (eq 'vacietis.c:* x1)
                                                     (eq 'vacietis.c:|,| x1)))
                                        (strip-type x1))))
                                (t
                                 (read-error
                                  "Junk in argument list: ~A" x)))))
                 (when (and (vectorp param) (c-type? (aref param 0)))
                   (dbg "  param: ~S~%" param)
                   (when (and (= (length param) 3) (equalp #() (aref param 2)))
                     (setq arg-type 'function-pointer))
                   #+nil
                   (push (lisp-type-declaration-for param)
                         arglist-type-declarations))
                 (loop for x across param do
                      (cond
                        ((eq x 'vacietis.c:|.|)
                         (progn (push '&rest            arglist)
                                (push 'vacietis.c:|...| arglist)
                                (return-from done-arglist)))
                        ((eq 'vacietis.c:* x)
                         (incf ptrlev))
                        ((and (listp x) (eq 'vacietis.c:[] (car x)))
                         ;; dimensions are reversed here
                         (setf arg-type
                               (make-array-type
                                :element-type arg-type
                                :dimensions (awhen (cadr x)
                                              (when (> (length it) 0)
                                                (list (aref it 0))))))
                         (dbg "  -> arg-type: ~S~%" arg-type)
                         (setf (gethash arg-name *local-variables*)
                               (make-c-variable :name arg-name
                                                :parameter t
                                                :type arg-type))
                         (setf (gethash arg-name *local-var-types*) arg-type))
                        ((not (or (c-type? x) (eq 'vacietis.c:* x)))
                         (strip-type x))))
                 ;; fix array dimensions
                 (let* ((type arg-type)
                        (dims))
                   (loop
                      while (array-type-p type)
                      do (when (array-type-p type)
                           (push (car (array-type-dimensions type)) dims)
                           (setq type (array-type-element-type type))))
                   (dbg "dims: ~S~%" dims)
                   (setq type arg-type)
                   (loop
                      while (array-type-p type)
                      do (when (array-type-p type)
                           (setf (array-type-dimensions type) (list (pop dims)))
                           (setq type (array-type-element-type type)))))
                 (push (lisp-type-declaration-for arg-type arg-name)
                       arglist-type-declarations))))))
    (if (eql (peek-char nil %in) #\;)
        (prog1 ;t
	    parameters
	  #+nil
	    (list arglist
		  arglist-type-declarations
		  local-arglist-declarations
		  local-arglist-lisp-type-declarations
		  *local-var-types*
		  *local-variables*)
	  (c-read-char)) ;; forward declaration
        (let ((ftype `(ftype (function ,(make-list (length arglist) :initial-element '*) ,(lisp-type-for result-type)) ,name)))
          (when (find '&rest arglist)
            (setq ftype nil))
          (setq arglist (nreverse arglist))
          (dbg "result type of ~S: ~S~%" name result-type)
          (dbg "ftype: ~S~%" ftype)
          (verbose "function ~S~%" ftype)
          (setf (gethash name (compiler-state-functions *compiler-state*))
                (make-c-function :return-type result-type
                                 :inline *is-inline*))
          `(progn
             ,@(when *is-inline* (list `(declaim (inline ,name))))
             ,@(when ftype (list `(declaim ,ftype)))
             (vac-defun/1 ,name ,arglist
               (declare ,@(remove-if #'null arglist-type-declarations))
               ,(let* ((*variable-declarations* ())
                       (*variable-lisp-type-declarations* ())
                       (body (read-c-block (next-char))))
                      `(prog* ,(append
                                local-arglist-declarations
                                (nreverse *variable-declarations*))
                          (declare
                           ,@(remove-if #'null local-arglist-lisp-type-declarations)
                           ,@(remove-if #'null *variable-lisp-type-declarations*))
                          ,@body))))))))

(defun one-long-progn (body)
  (loop for x in body
     nconc (cond
             ((and (listp x) (eq 'progn (car x)))
              (one-long-progn (cdr x)))
             (t
              (list x)))))

(defun get-alien-value-types (type)
  (cond
    ((array-type-p type)
     (let* ((dimensions   (lisp-array-dimensions type))
            (element-type (lisp-array-element-type type))
            (depth        (1- (length dimensions)))
            (indices      (make-list (1+ depth) :initial-element 0)))
       (labels ((do-nth-dimension (n)
                  (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (get-alien-value-types element-type)
                                 (do-nth-dimension (1+ n))))))
         (do-nth-dimension 0))))
    ((struct-type-p type)
     (map-struct-slots
      (lambda (name type)
        (declare (ignore name))
        (get-alien-value-types type))
      type))
    (t
     type)))

(defun set-alien-values (type alien next-value next-offset)
  (cond
    ((array-type-p type)
     (let* ((dimensions   (lisp-array-dimensions type))
            (element-type (lisp-array-element-type type))
            (depth        (1- (length dimensions)))
            (indices      (make-list (1+ depth) :initial-element 0)))
       ;;(dbg "set-alien-values dimensions: ~S~%" dimensions)
       (labels ((do-nth-dimension (n)
                  (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (let (#+nil (indexen (copy-list indices)))
                                   (set-alien-values element-type
                                                     alien
                                                     #+nil
                                                     `(sb-alien:deref ,alien ,@indexen)
                                                     next-value next-offset))
                                 `(progn ,@(do-nth-dimension (1+ n)))))))
         `(progn ,@(do-nth-dimension 0)))))
    ((struct-type-p type)
     `(progn
        ,@(map-struct-slots
           (lambda (name type)
             (declare (ignore name))
             (set-alien-values type
                               alien
                               #+nil
                               `(sb-alien:slot ,alien ',name)
                               next-value next-offset))
           type)))
    (t
     ;;(dbg "set-alien-values type: ~S~%" type)
     (let ((setter (sap-set-ref-for type)))
       `(,setter ,alien
                 ,(funcall next-offset type)
                 (the ,(lisp-type-for type) ,(funcall next-value type))))
     #+nil
     `(setf ,alien ,(funcall next-value type)))))

(defun vac-arithmetic-expression? (expr)
  (cond
    ((or (numberp expr)
         ;; XXX bit shifts, what else?
         (member expr '(cl:+ cl:- cl:* cl:/ cl:mod))
         (member expr '(vacietis.c:+ vacietis.c:- vacietis.c:* vacietis.c:/ vacietis.c:%)))
     expr)
    ((listp expr)
     (dolist (e expr expr)
       (unless (vac-arithmetic-expression? e)
         (return nil))))
    (t
     nil)))

(defun flatten-vector-literal (value)
  (loop for x in (cond ((vector-literal-p value)
                        (vector-literal-elements value))
                       (t
                        value))
     nconc (cond
             ((null x)
              nil)
             ((vector-literal-p x)
              (flatten-vector-literal x))
             ((atom x)
              (list x))
             ((vac-arithmetic-expression? x)
              (list x))
             (t
              (flatten-vector-literal x)))))

(defmacro vac-arithmetic-expression-override (&body body)
  `(macrolet ((vacietis.c:+ (&rest rest)
                `(+ ,@rest))
              (vacietis.c:- (&rest rest)
                `(- ,@rest))
              (vacietis.c:* (&rest rest)
                `(* ,@rest))
              (vacietis.c:/ (&rest rest)
                `(/ ,@rest)))
     ,@body))

(defmacro evaluate-arithmetic-expression (expr)
  `(vac-arithmetic-expression-override
     (let ((expanded-body (macroexpansion-of ,expr)))
       ;;(dbg "expanded-body: ~S~%" expanded-body)
       (eval expanded-body))))

(defun convert-to-alien-value-function (type)
  (let ((offset 0)
        (n-elements 0))
    (labels ((next-value (type)
               (declare (ignore type))
               `(pop elements))
             (next-offset (type)
               (prog1 `(+ offset ,offset)
                 (incf n-elements)
                 (incf offset (eval `(sb-alien:alien-size ,(alien-type-for type) :bytes))))))
      (values
       `(lambda (sap offset elements)
          (declare ,*optimize*
                   (type sb-sys:system-area-pointer sap)
                   (type fixnum offset)
                   (type list elements))
          ,(one-long-progn (set-alien-values type 'sap #'next-value #'next-offset)))
       n-elements))))

(defvar *alien-value-ctr* 0)
(defvar *alien-value-table* (make-hash-table :weakness :value))

(defun convert-to-alien-value (type value)
  (let* ((c-pointer (gensym))
         (sap (gensym))
         (offset 0)
         (size (gensym))
         (alien-type (alien-type-for type))
         (alien-value-ctr (incf *alien-value-ctr*))
         (elements (flatten-vector-literal value))
         (elements-are-constant (arithmetic-expression? elements)))
    (dbg "convert-to-alien: alien type: ~S~%" alien-type)
    (dbg "  -> elements-are-constant: ~S~%" (if elements-are-constant t nil))
    (when elements-are-constant
      (setq elements (eval (list* 'list elements))))
    ;;(dbg "convert-to-alien: flattened: ~S~%" elements)
    (labels ((next-value (type)
               (let ((value (pop elements)))
                 (cond
                   ((and (eq type 'vacietis.c:float) (typep value 'double-float))
                    (coerce value 'single-float))
                   ((and (constantp value) (numberp value))
                    (lisp-constant-value-for type value))
                   ((vac-arithmetic-expression? value)
                    (eval `(evaluate-arithmetic-expression ,value)))
                   (t
                    value))))
             (next-offset (type)
               (prog1 offset
                 (incf offset (eval `(sb-alien:alien-size ,(alien-type-for type) :bytes))))))
      (if (array-type-p type)
          (let* ((idx (gensym))
                 (offset (gensym))
                 (all-elements (gensym))
                 (element-size (gensym))
                 (convert-one (gensym))
                 (element-type (array-type-element-type type))
                 (alien-element-type (alien-type-for element-type)))
            (dbg "  -> array dimensions: ~S~%" (array-type-dimensions type))
            (when elements-are-constant
              (let ((types (get-alien-value-types element-type)))
                (when (atom types)
                  (setq types (list types)))
                (dbg "  -> types: ~S~%" (get-alien-value-types element-type))
                (let* ((loop-types types)
                       (elements
                       (loop
                          for value = (let ((type (car loop-types)))
                                        (when (endp loop-types)
                                          (setq loop-types types))
                                        (next-value type))
                          while value
                          collect value)))
                  ;;(format t " -> elements: ~S~%" elements)
                  (setf (gethash alien-value-ctr *alien-value-table*) elements))))
            (multiple-value-bind (convert-one-form n-elements)
                (convert-to-alien-value-function element-type)
              `(locally
                   (declare ,*optimize*)
                 (let* ((,size (sb-alien:alien-size ,alien-type :bytes))
                        (,element-size (sb-alien:alien-size ,alien-element-type :bytes))
                        (,offset 0)
                        (,c-pointer (array-backed-c-pointer ,size)))
                   (labels ((,convert-one ,@(cdr convert-one-form)))
                     (with-array-backed-c-pointers (,c-pointer)
                       (let ((,sap (c-pointer-sap ,c-pointer))
                             (,all-elements
                              ,(if elements-are-constant
                                   `(gethash ,alien-value-ctr *alien-value-table*)
                                   (list* 'list elements))))
                         (loop for ,idx from 0 upto ,(1- (car (array-type-dimensions type)))
                            while ,all-elements
                            do (progn
                                 ;;(format t "(convert-one '(~S)~%" (subseq ,all-elements 0 ,(1- n-elements)))
                                 (,convert-one
                                  ,sap
                                  ,offset
                                  ,all-elements)
                                 (setq ,all-elements (nthcdr ,n-elements ,all-elements))
                                 (incf ,offset ,element-size)))
                         ,c-pointer)))))))
          `(locally
               (declare ,*optimize*)
             (let* ((,size (sb-alien:alien-size ,alien-type :bytes))
                    (,c-pointer (array-backed-c-pointer ,size)))
               (with-array-backed-c-pointers (,c-pointer)
                 (let ((,sap (c-pointer-sap ,c-pointer)))
               ,(one-long-progn (set-alien-values type sap #'next-value #'next-offset))))
               ,c-pointer))))))

(defun to-struct-value (type value)
  (if *use-alien-types*
      (convert-to-alien-value type value)
      (let ((row (map 'list #'identity (vector-literal-elements value))))
        (dbg "lisp-type: ~S~%" (lisp-type-for type))
        (dbg "row: ~S~%" row)
        (let* ((lisp-type (lisp-type-for type))
               (element-type (cadr lisp-type)))
          `(make-array ,(length row)
                       :element-type ',element-type
                       :initial-contents
                       ,(list*
                         'list
                         (labels ((next-value (type)
                                    (let ((value (pop row)))
                                      (dbg "to-struct-value type: ~S value: ~S~%" type value)
                                      (cond
                                        ((struct-type-p type)
                                         (to-struct-value type value))
                                        ((array-type-p type)
                                         (dbg "array-element-type: ~S~%" (lisp-type-for (array-type-element-type type)))
                                         `(make-array ',(array-type-dimensions type)
                                                      :element-type ',(lisp-type-for (array-type-element-type type))
                                                      :initial-contents
                                                      ,(list* 'list (get-elements (array-type-element-type type) (array-type-dimensions type) value))))
                                        ((and (constantp value) (numberp value))
                                         (lisp-constant-value-for type value))
                                        ((arithmetic-expression? value)
                                         (eval value))
                                        (t
                                         value)))))
                           (map-struct-slots
                            (lambda (name type)
                              (next-value type))
                            type))))))))

(defun get-dimensions (name1 &optional dimensions)
  (dbg "get-dimensions name1: ~S~%" name1)
  (let ((dim1 (third name1)))
    (if (listp (second name1))
      (nconc dimensions (get-dimensions (second name1)) (list dim1))
      (nconc dimensions (list dim1)))))

(defun arithmetic-expression? (expr)
  (cond
    ((or (numberp expr)
         ;; XXX bit shifts, what else?
         (member expr '(cl:+ cl:- cl:* cl:/ cl:mod)))
     expr)
    ((listp expr)
     (dolist (e expr expr)
       (unless (arithmetic-expression? e)
         (return nil))))
    (t
     nil)))

(defun get-elements (base-type dimensions value)
  (if (= 1 (length dimensions))
      (map 'list #'(lambda (x)
                     ;;(dbg "element value: ~S~%" x)
                     (if (vector-literal-p x)
                         (let ((elements (vector-literal-elements x)))
                           (if (and (not *use-alien-types*) (struct-type-p base-type))
                               (to-struct-value base-type x)
                               (list* 'list (get-elements base-type (list (length elements)) x))))
                         (cond
                           ((and (constantp x) (numberp x))
                            (lisp-constant-value-for base-type x))
                           ((arithmetic-expression? x)
                            (eval x))
                           (t
                            x))))
           (vector-literal-elements value))
      (map 'list #'(lambda (x)
                     (list* 'list (get-elements base-type (cdr dimensions) x)))
           (vector-literal-elements value))))

(defun to-lisp-array (type base-type name1 value)
  (let* ((dimensions (get-dimensions name1))
         (lisp-element-type (lisp-type-for base-type))
         (elements (remove-if #'null (get-elements base-type dimensions value))))
    (dbg "to-lisp-array: dimensions: ~S~%" dimensions)
    (when (null (car dimensions))
      (setf dimensions (list (length elements)))
      (unless (array-type-dimensions type)
        (setf (array-type-dimensions type)
              (if (listp dimensions)
                  dimensions
                  (list dimensions)))))
    (if (null (car dimensions))
        (if *use-alien-types*
            (convert-to-alien-value type value)
            (values `(make-array ,(length elements)
                                 :element-type ',lisp-element-type
                                 :initial-contents (list ,@(map 'list #'identity elements)))
                    (length elements)))
        (progn
          (dbg "making array of dimensions ~S~%" dimensions)
          ;;(dbg "elements: ~S~%" elements)
          (if *use-alien-types*
              (convert-to-alien-value type value)
              (values `(make-array ',dimensions
                                   :element-type ',lisp-element-type
                                   :initial-contents (list ,@(map 'list #'identity elements)))
                      dimensions))))))

;; for an array of struct typed objects
(defun pass2-struct-array (type array)
  (loop for i from 0 upto (1- (length array)) do
       (let ((row (aref array i)))
         (loop for j from 0 upto (1- (length row)) do
              (let ((it (aref row j)))
                (cond
                  ((vectorp it)
                   (let* ((slot-type (nth j (slot-value type 'slots)))
                          (element-type (slot-value slot-type 'element-type)))
                     ;;(dbg "element-type: ~S (~S)~%" element-type (length it))
                     (setf (aref row j)
                           (make-array (length it)
                                       :element-type (lisp-type-for element-type)
                                       :initial-contents it))))))))))

(defvar *in-struct* nil)

(defun process-variable-declaration (spec base-type)
  (let (name (type base-type) initial-value init-size)
    (labels ((init-object (name1 value)
               (if (vector-literal-p value)
                   (let ()
                     (dbg "variable declaration of ~S: type: ~S name1: ~S~%" name type name1)
                     (if (struct-type-p type)
                         (if (symbolp name)
                             (to-struct-value type value)
                             (multiple-value-bind (array dimensions)
                                 (to-lisp-array type base-type name1 value)
                               (when type
                                 (unless (array-type-dimensions type)
                                   (setf (array-type-dimensions type)
                                         (if (listp dimensions)
                                             dimensions
                                             (list dimensions)))))
                               ;;(unless *use-alien-types*
                               ;;  (pass2-struct-array type array))
                               (dbg "set dimensions to: ~S~%" dimensions)
                               (dbg "struct-type: ~S~%" type)
                               (setf init-size dimensions)
                               array))
                         (progn
                           (multiple-value-bind (array dimensions)
                               (to-lisp-array type base-type name1 value)
                             (when type
                               (unless (array-type-dimensions type)
                                 (setf (array-type-dimensions type)
                                       (if (listp dimensions)
                                           dimensions
                                           (list dimensions)))))
                             (dbg "set dimensions to: ~S~%" dimensions)
                             (dbg "array-type: ~S~%" type)
                             (setf init-size dimensions)
                             array))))
                   (progn
                     (when (and (listp value) (eq 'string-to-char* (car value)))
                       (setf init-size (1+ (length (second value)))))
                     (progn
                       (dbg "initial value of ~S (type ~S): ~S~%" name type value)
                       value))))
             (parse-declaration (x)
               (if (symbolp x)
                   (setf name x)
                   (destructuring-bind (qualifier name1 &optional val/size)
                       x
                     (setf name name1)
                     #+nil
                     (unless (eq 'vacietis.c:alien[] qualifier)
                       (setq val/size name1)
                       (setq name1 name0))
                     (dbg "qualifier: ~S~%" qualifier)
                     (cond
                       ((eq 'vacietis.c:= qualifier)
                        (parse-declaration name1)
                        (setf initial-value (init-object name1 val/size)))
                       ((or (eq 'vacietis.c:[] qualifier) (eq 'vacietis.c:alien[] qualifier))
                        (setf type
                              (make-array-type
                               :in-struct *in-struct*
                               :element-type type
                               :dimensions   (awhen (or val/size init-size)
                                               (dbg "array dimensions: ~S~%" it)
                                               (when (arithmetic-expression? it)
                                                 (list (eval it))))))
                        (dbg "array set type to ~S~%" type)
                        (parse-declaration name))
                       ((eq 'vacietis.c:deref* qualifier)
                        (setf type (make-pointer-to :type type))
                        (dbg "set type to ~S~%" type)
                        ;;XXX what about actual pointers to pointers?
                        (unless (or (null initial-value)
                                    (and (listp initial-value)
                                         (eq (car initial-value) 'vacietis.c:mkptr&)))
                          (setq initial-value `(vacietis.c:mkptr& (aref ,initial-value 0))))
                        ;; skip type
                        (when *use-alien-types*
                          (setq name val/size))
                        (parse-declaration name))
                       (t (read-error "Unknown thing in declaration ~A" x)))))))
      (dbg "spec: ~S~%" spec)
      (parse-declaration spec)
      (values name type initial-value))))

(defun read-variable-declarations (spec-so-far base-type)
  (let* ((*variable-declarations-base-type* base-type)
         (decls      (c-read-delimited-list #\; #\,))
         (decl-code  ()))
    (setf (aref decls 0) (concatenate 'vector spec-so-far (aref decls 0)))
    ;;(dbg "rvd: spec-so-far: ~S ~S~%" spec-so-far base-type)
    (loop for x across decls
       do
         (multiple-value-bind (name type initial-value)
             (process-variable-declaration (parse-infix x 0 (length x) base-type) base-type)
           (dbg "setting local-var-type of ~S  (extern: ~S)  to ~S~%" name *is-extern* type)
           ;;(dbg "  -> initial-value: ~S~%" initial-value)
           (setf (gethash name (or *local-var-types*
                                   (compiler-state-var-types *compiler-state*)))
                 type)
           (setf (gethash name (or *local-variables*
                                   (compiler-state-variables *compiler-state*)))
                 (make-c-variable :name name
                                  :type type))
           (if (boundp '*variable-declarations*)
               (let* ((value-type type)
                      (value (if initial-value
                                 (if (and *use-alien-types* (pointer-to-p type))
                                     `(copy-c-pointer ,initial-value)
                                     initial-value)
                                 (preallocated-value-exp-for type))))
                 (push `(,name ,@(when value (list value)))
                       *variable-declarations*)
                 (when (or (pointer-to-p value-type)
                           (array-type-p value-type))
                   (push `(dynamic-extent ,name)
                         *variable-lisp-type-declarations*))
                 (dbg "variable declaration: ~S ~S~%" type name)
                 (push (lisp-type-declaration-for type name)
                       *variable-lisp-type-declarations*))
               (unless *is-extern*
                 ;;(dbg "global ~S type: ~S initial-value: ~S~%" name type initial-value)
                 (cond
                   ((and (primitive-type? type)
                         (not (pointer-to-p type)))
                    (let* ((alien-type (alien-type-for type))
                           (size (* 3 (eval `(sb-alien:alien-size ,alien-type :bytes))))
                           (getter (sap-get-ref-for type))
                           (buffer-tmpsym (gensym))
                           (varname name)
                           (varvalue (or initial-value
                                         (preallocated-value-exp-for type)))
                           (pointer-sym (intern (format nil "POINTER-~A" (symbol-name varname)))))
                      (push `(declaim (type c-pointer ,pointer-sym))
                            decl-code)
                      (push `(locally (declare ,*optimize*)
                               (let ((,buffer-tmpsym (make-array ,size :element-type '(unsigned-byte 8))))
                                 (defparameter ,pointer-sym (array-backed-c-pointer-of ,buffer-tmpsym))
                                 (setf (,getter (c-pointer-sap ,pointer-sym) (c-pointer-offset ,pointer-sym))
                                       ,varvalue)))
                            decl-code)
                      (eval `(define-symbol-macro ,varname (,getter (c-pointer-sap ,pointer-sym) (c-pointer-offset ,pointer-sym))))
                      (push `(define-symbol-macro ,varname (,getter (c-pointer-sap ,pointer-sym) (c-pointer-offset ,pointer-sym)))
                            decl-code)))
                   (t
                    (let* ((defop 'defparameter)
                           (varname name)
                           (varvalue (or initial-value
                                         (preallocated-value-exp-for type)))
                           (declamation `(declaim (type ,(lisp-type-for type) ,varname))))
                      (dbg "declamation: ~S~%" declamation)
                      (verbose "global ~S~%" declamation)
                      (push declamation
                            decl-code)
                      (push `(locally (declare ,*optimize*)
                               (,defop ,varname
                                   ,varvalue))
                            decl-code))))))))
    (if decl-code
        (cons 'progn (nreverse decl-code))
        t)))

(defun read-var-or-function-declaration (base-type)
  "Reads a variable(s) or function declaration"
  (dbg "read-var-or-function-declaration: ~S~%" base-type)
  
  (let ((type base-type)
        name
        (spec-so-far (make-buffer)))
    (loop for c = (next-char) do
         (cond ((eql c #\*)
                (setf type        (make-pointer-to :type type))
                (vector-push-extend 'vacietis.c:* spec-so-far))
               ((or (eql c #\_) (alpha-char-p c))
                (setf name (read-c-identifier c))
                (vector-push-extend name spec-so-far)
                (return))
               (t
                (c-unread-char c)
                (return))))
 ;   (format t "~&~a" name)
    (let ((next (next-char)))
      (c-unread-char next)
      (if (and name (eql #\( next))
          (let ((foo (read-function name type)))
		 (list 'defun name type foo))
          (read-variable-declarations spec-so-far base-type)))))

(defun read-enum-decl ()
  (when (eql #\{ (peek-char t %in))
    (next-char)
    (let ((enums (c-read-delimited-list #\{ #\,)))
      ;; fixme: assigned values to enum names
      (loop for name across enums for i from 0 do
           (setf (gethash (elt name 0) (compiler-state-enums *compiler-state*))
                 i))))
  (if (eql #\; (peek-char t %in))
      (progn (next-char) t)
      (read-variable-declarations #() 'vacietis.c:int)))

(defun modify-base-type (base-type)
  (cond
    (*is-unsigned*
     (case base-type
       (vacietis.c:char   'vacietis.c:unsigned-char)
       (vacietis.c:short  'vacietis.c:unsigned-short)
       (vacietis.c:int    'vacietis.c:unsigned-int)
       (vacietis.c:long   'vacietis.c:unsigned-long)
       (t base-type)))
    (t base-type)))

(defun read-base-type (token)
  (dbg "read-base-type: ~S~%" token)
  (let (fuck)
    (loop while (type-qualifier? token)
       do
	 (dbg "type qualifier token: ~S~%" token)
	 (case token
	   (vacietis.c:extern
	    (setq *is-extern* t))
	   (vacietis.c:inline
	    (setq *is-inline* t))
	   (vacietis.c:unsigned
	    (setq *is-unsigned* t))
	   (vacietis.c:const
	    (setq *is-const* t)))
	 (setf fuck (file-position %in))
	 (setf token (next-exp)))
    #+nil
    (awhen (gethash token (compiler-state-typedefs *compiler-state*))
      (setf token it))
    (cond ((eq token 'vacietis.c:enum)
	   (values (make-enum-type :name (next-exp)) t))
	  ((eq token 'vacietis.c:struct)
	   (dbg "  -> struct~%")
	   (if (eql #\{ (peek-char t %in))
	       (progn
		 (c-read-char)
		 (values (read-struct-decl-body (make-struct-type)) t))
	       (let ((name (next-exp)))
		 (dbg "  -> struct name: ~S~%" name)
		 (values (or (gethash name (compiler-state-structs *compiler-state*))
			     (make-struct-type :name name))
			 t))))
	  ((or (basic-type? token) (c-type-p token))
	   (values (modify-base-type token) nil))
	  (t
	   (values
	    (if *is-unsigned*
		(prog1 'vacietis.c:unsigned-int
		  (file-position %in fuck))
		token)
	    nil)
	   #+nil
	   (read-error "Unexpected parser error: unknown type ~A" token)))))

(defun read-struct-decl-body (struct-type)
  (let ((slot-index 0)
        (*in-struct* t))
    (loop for c = (next-char) until (eql #\} c) do
         (multiple-value-bind (slot-name slot-type)
             (let ((base-type (read-base-type (read-c-exp c))))
               (process-variable-declaration (read-infix-exp (next-exp))
                                             base-type))
           (dbg "struct-type: ~S slot-name ~A i: ~D~%" struct-type slot-name slot-index)
           (setf (gethash (format nil "~A.~A" (slot-value struct-type 'name) slot-name)
                          (compiler-state-accessors *compiler-state*))
                 (if *use-alien-types* slot-name slot-index)
                 (struct-type-slot-names struct-type)
                 (append (struct-type-slot-names struct-type) (list slot-name))
                 (struct-type-slots struct-type)
                 (append (struct-type-slots struct-type) (list slot-type)))
           (incf slot-index)))))

(defun read-c-identifier-list (c)
  (map 'list (lambda (v)
               (if (= 1 (length v))
                   (aref v 0)
                   v))
       (c-read-delimited-list #\; #\,)))

(defun read-struct (struct-type &optional for-typedef)
  (acase (next-char)
    (#\{ (read-struct-decl-body struct-type)
         (awhen (struct-type-name struct-type)
           (setf (gethash it (compiler-state-structs *compiler-state*))
                 struct-type))
         (let ((c (next-char)))
           (if (eql #\; c)
               t
               (progn (c-unread-char c)
                      (if for-typedef
                          (read-c-identifier-list c)
                          (read-variable-declarations #() struct-type))))))
    (#\; t) ;; forward declaration
    (t   (if for-typedef
             (progn (c-unread-char it)
                    (read-c-identifier-list it))
             (read-variable-declarations (vector (read-c-exp it))
                                         struct-type)))))

(defun read-infix-exp (next-token)
  (let ((exp (make-buffer)))
    (vector-push-extend next-token exp)
    (loop for c = (next-char nil)
          until (or (eql c #\;) (null c))
          do (vector-push-extend (read-c-exp c) exp))
    (parse-infix exp)))

(defun read-typedef (base-type)
  (dbg "read-typedef: ~S~%" base-type)
  (cond ((struct-type-p base-type)
         (let ((names (read-struct base-type t)))
           (dbg "typedef read-struct names: ~S~%" names)
	   #+nil
           (dolist (name names)
             (when (symbolp name) ;; XXX handle pointer and array typedefs
               (setf (gethash name (compiler-state-typedefs *compiler-state*)) base-type)))
	   (list names
		 base-type)
           ;t
	   ))
        (t
	 (let* ((token (next-exp))
		(wot (read-infix-exp token)))
;	   (print wot)
	   (list token
		 base-type)
	   #+nil
	   (multiple-value-bind (name type)
	       (process-variable-declaration wot base-type)
	     (declare (ignorable name type))
	     #+nil
	     (setf (gethash name (compiler-state-typedefs *compiler-state*)) type)
					;t
	     )))))

(defun read-declaration (token)
  (cond ((eq 'vacietis.c:typedef token)
         (let ((*is-unsigned* nil))
           (let* ((*is-extern* nil)
                  (*is-const* nil)
                  (*is-unsigned* nil)
                  (base-type (read-base-type (next-exp))))
             (read-typedef base-type)
	     )))
        (t ;(c-type? token)
         (let* ((*is-inline* nil)
                (*is-extern* nil)
                (*is-const* nil)
                (*is-unsigned* nil))
           (multiple-value-bind (base-type is-decl)
               (read-base-type token)
             (dbg "read-declaration base-type: ~S~%" base-type)
;	     (format t "~&read-declaration base-type: ~S~% ~S~% ~S~%" base-type token is-decl)
             (if is-decl
                 (cond ((struct-type-p base-type)
                        (read-struct base-type))
                       ((enum-type-p base-type)
                        (read-enum-decl)))
                 (read-var-or-function-declaration base-type)))))))

(defun read-labeled-statement (token)
  (when (eql #\: (peek-char t %in))
    (next-char)
    (values (read-c-statement (next-char)) token)))

(defun read-infix-exp (next-token)
  (let ((exp (make-buffer)))
    (vector-push-extend next-token exp)
    (loop for c = (next-char nil)
          until (or (eql c #\;) (null c))
          do (vector-push-extend (read-c-exp c) exp))
    (parse-infix exp)))

(defun %read-c-statement (token)
  (multiple-value-bind (statement label) (read-labeled-statement token)
    (if label
	(values statement label)
	(let ((foobar (read-declaration token)))
	  (if foobar
	      (if (eq t foobar)
		  (values)
		  foobar)
	      (or (read-control-flow-statement token)
		  (read-infix-exp token)))))))

(defun read-c-statement (c)
;  (print c)
  (case c
    (#\# (read-c-macro %in c))
    (#\; (values))
    (t
  ;   (print (file-position %in))
     (let ((wot (read-c-exp c)))
  ;     (print (file-position %in))
       (%read-c-statement wot)))))

(defun read-c-identifier (c)
  ;; assume inverted readtable (need to fix for case-preserving lisps)
  (let* ((raw-name (concatenate
                    'string (string c)
                    (slurp-while (lambda (c)
                                   (or (eql c #\_) (alphanumericp c))))))
         (raw-name-alphas (remove-if-not #'alpha-char-p raw-name))
         (identifier-name
          (format nil
                  (cond ((every #'upper-case-p raw-name-alphas) "~(~A~)")
                        ((every #'lower-case-p raw-name-alphas) "~:@(~A~)")
                        (t "~A"))
                  raw-name)))
    ;;(dbg "identifier-name: ~S~%" identifier-name)
    (let ((symbol (or (find-symbol identifier-name '#:vacietis.c)
                      (intern identifier-name)
                      ;;(intern (string-upcase identifier-name))
                      )))
      (cond
        ((eq 't symbol) 'c-t)
        (t symbol)))))

(defun match-longest-op (one)
  (flet ((seq-match (&rest chars)
           (find (make-array (length chars)
                             :element-type 'character
                             :initial-contents chars)
                 *ops* :test #'string= :key #'symbol-name)))
    (let ((one-match (seq-match one))
          (two (c-read-char)))
      (acond ((null two)
              one-match)
             ((seq-match one two)
              (let ((three-match (seq-match one two (peek-char nil %in))))
                (if three-match
                    (progn (c-read-char) three-match)
                    it)))
             (t (c-unread-char two) one-match)))))

(defstruct vector-literal
  elements)

(defun read-vector-literal ()
  (make-vector-literal
   :elements (map 'list #'parse-infix (c-read-delimited-list #\{ #\,))))

(defun read-c-exp (c)
 ; (print c)
 ; (print (file-position %in))
  (or (match-longest-op c)
      ;(progn (print (file-position %in)))
      (cond ((digit-char-p c) (read-c-number c))
	    ((or (eql c #\_)
		 (alpha-char-p c))
	     (let ((symbol (read-c-identifier c)))
	       ;;(dbg "~S -> symbol: ~S~%" c symbol)
	       #+nil
	       (when (eq t symbol)
		 (setq symbol '__c_t))
;	       (print %in)
	       (acond
		 ((gethash symbol (compiler-state-pp *compiler-state*))
		  ;;(describe it)
		  #+nil
		  (setf *macro-stream*
			(make-string-input-stream
			 (etypecase it
			   (string
			    it)
			   (function
			    (funcall it (c-read-delimited-strings)))))
			%in
			(make-concatenated-stream *macro-stream* %in))
		  ;;(dbg "read-c-exp...~%")
	;	  (print %in)
	;	  (print (file-position %in))
		  (let ((a-char (next-char)))
	;	    (print a-char)
		    (read-c-exp a-char)))
		 ((gethash symbol (compiler-state-enums *compiler-state*))
		  ;;(dbg "returning it...~%")
		  it)
		 (t
		  symbol))))
	    (t
	     (case c
	       (#\" (read-c-string %in c))
	       (#\' (read-character-constant %in c))
	       (#\( (read-exps-until (lambda (c) (eql #\) c))))
	       (#\{ (read-vector-literal)) ;; decl only
	       (#\[ (list 'vacietis.c:[]
			  (read-exps-until (lambda (c) (eql #\] c))))))))))

;;; readtable

(defun read-c-toplevel (%in c)
  (print 12312312)
  (let* ((*macro-stream* nil)
         (exp1           (read-c-statement c)))
    ;;(dbg "toplevel: ~S~%" exp1)
    (if (and *macro-stream* (peek-char t *macro-stream* nil))
        (list* 'progn
               exp1
               (loop while (peek-char t *macro-stream* nil)
                     collect (read-c-statement (next-char))))
        (or exp1 (values)))))

(macrolet
    ((def-c-readtable ()
       `(defreadtable c-readtable
         (:case :invert)

         ;; unary and prefix operators
         ,@(loop for i in '(#\+ #\- #\~ #\! #\( #\& #\*)
              collect `(:macro-char ,i 'read-c-toplevel nil))

         (:macro-char #\# 'read-c-macro nil)

         (:macro-char #\/ 'read-c-comment nil)

         (:macro-char #\" 'read-c-string nil)
         (:macro-char #\' 'read-character-constant nil)

         ;; numbers (should this be here?)
         ,@(loop for i from 0 upto 9
              collect `(:macro-char ,(digit-char i) 'read-c-toplevel nil))

         ;; identifiers
         (:macro-char #\_ 'read-c-toplevel nil)
         ,@(loop for i from (char-code #\a) upto (char-code #\z)
              collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         ,@(loop for i from (char-code #\A) upto (char-code #\Z)
              collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         )))
  (def-c-readtable))

(defvar c-readtable (find-readtable 'c-readtable))

;;; reader

(defun cstr (str)
  (dbg "cstr: ~S~%" str)
  (with-input-from-string (s str)
    (let ((*compiler-state* (make-compiler-state))
          (*readtable*      c-readtable))
      (let ((body (cons 'progn (loop for it = (read s nil 'eof)
                                  while (not (eq it 'eof)) collect it))))
        (eval `(vac-progn/1 ,body))))))

(defun cstr-noeval (str)
  (with-input-from-string (s str)
    (let ((*compiler-state* (make-compiler-state))
          (*readtable*      c-readtable))
      (let ((body (cons 'progn (loop for it = (read s nil 'eof)
                                  while (not (eq it 'eof)) collect it))))
        body))))

(defun %load-c-file (*c-file* *compiler-state*)
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
    (load *c-file*)))

(defun load-c-file (file)
  (%load-c-file file (make-compiler-state)))

(defun wow (*c-file* &optional (*compiler-state* (make-compiler-state)))
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
    (with-open-file (stream *c-file*)
      (let ((eof (list nil)))
	(loop
	   (let ((value
		  (read stream nil eof)))
	     (if (eq value eof)
		 (return)
		 (print value)
		 )))))))
