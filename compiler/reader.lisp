(in-package #:vacietis)

;;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (debug 3) (safety 1)))

;;(defparameter *optimize* '(optimize (speed 0) (debug 3) (safety 3)))
;;(defparameter *optimize* '(optimize (speed 3) (debug 0) (safety 1)))
(defparameter *optimize* '(optimize (speed 3) (debug 0) (safety 0)))

(in-package #:vacietis.c)

(cl:defparameter vacietis::*type-qualifiers*
  #(inline static const
    signed unsigned
    extern auto register
    long short
    float double int char
    void *))

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

(symbol-macrolet ((body (gethash def
				 (compiler-state-pp *compiler-state*))))
  (defun (setf get-pp-def) (var def)
    (setf
     body
     var)) 
  (defun get-pp-def (def)
    body))

(defmacro lookup-define ()
  `(get-pp-def (read-c-identifier (next-char))))

(defmacro defined (value)
  `(get-pp-def ,(string value)))

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
    (let ((wtf
	   `(symbol-macrolet
		,(let ((x))
		      (maphash (lambda (k v)
				 (push (list k v) x))
			       (compiler-state-pp *compiler-state*))
		      x)
	      ,exp)))
					;     (print wtf)
      (eval wtf))))

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
	 (declare (ignore include-file))
         (next-char)
	 #+nil
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
      #+nil
      (find identifier #(vacietis.c:struct vacietis.c:enum))
      #+nil
      (gethash identifier (compiler-state-typedefs *compiler-state*))))

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

(defvar *function-name*)
(defvar *local-var-types*)
(defvar *local-variables*)

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
	  #+nil
          (setf (gethash name (compiler-state-functions *compiler-state*))
                (make-c-function :return-type result-type
                                 :inline *is-inline*))
	  parameters
	  #+nil
	  (let ((body (read-c-block (next-char))))
	    `(progn
	       ,@(when *is-inline* (list `(declaim (inline ,name))))
	       ,@(when ftype (list `(declaim ,ftype)))
	       (vac-defun/1 ,name ,arglist
			    (declare ,@(remove-if #'null arglist-type-declarations))
			    ,(let* ((*variable-declarations* ())
				    (*variable-lisp-type-declarations* ()))
				   `(prog* ,(append
					     local-arglist-declarations
					     (nreverse *variable-declarations*))
				       (declare
					,@(remove-if #'null local-arglist-lisp-type-declarations)
					,@(remove-if #'null *variable-lisp-type-declarations*))
				       ,@body)))))))))


(defun read-variable-declarations (spec-so-far base-type)
  (let* (;(*variable-declarations-base-type* base-type)
         (decls      (c-read-delimited-list #\; #\,))
	 #+nil
         (decl-code  ()))
    (setf (aref decls 0) (concatenate 'vector spec-so-far (aref decls 0)))
    ;;(dbg "rvd: spec-so-far: ~S ~S~%" spec-so-far base-type)
    #+nil
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
    (print decls)
    #+nil
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
         (cond
	   #+nil
	   ((eql c #\*)
	    (setf type (make-pointer-to :type type))
	    (vector-push-extend 'vacietis.c:* spec-so-far))
	   ((or (eql c #\_)
		(alpha-char-p c))
	    (setf name (read-c-identifier c))
	    (vector-push-extend name spec-so-far)
	    (return))
	   (t
	    (c-unread-char c)
	    (return))))
;;;;					    (format t "~&~a" name)
					;    (print (file-position %in))
    (let ((next (next-char)))
      (c-unread-char next)
      ;;(print next)
      (if (and name (eql #\( next))
          (let ((foo (read-function name type)))
	    (list 'defcfun type name foo))
          (read-variable-declarations spec-so-far base-type)))))

(defun read-enum-decl ()
  (let ((ret :no-enum-here))
    (when (eql #\{ (peek-char t %in))
      (next-char)
      (let ((enums (c-read-delimited-list #\{ #\,)))
	(setf ret enums)
	;; fixme: assigned values to enum names
	#+nil
	(loop for name across enums for i from 0 do
	     (setf (gethash (elt name 0) (compiler-state-enums *compiler-state*))
		   i))))
    (if (eql #\; (peek-char t %in))
	(progn (next-char)
	       ret
	       ;;t
	       )
	(read-variable-declarations #() 'vacietis.c:int))))


(defun mehtype (tokens)
  (multiple-value-bind (token)
      (cond ((find 'vacietis.c:char tokens) ;;char
	     (if (find 'vacietis.c:unsigned tokens)
		 'vacietis.c:unsigned-char
		 'vacietis.c:char))
	    ((find 'vacietis.c:short tokens) ;;short
	     (if (find 'vacietis.c:unsigned tokens)
		 'vacietis.c:unsigned-short
		 'vacietis.c:short))
	    ((find 'vacietis.c:long tokens) ;;longs
	     (if (< 1 (count 'vacietis.c:long tokens))
		 (if (find 'vacietis.c:unsigned tokens)
		     'vacietis.c:unsigned-long-long
		     'vacietis.c:long-long)
		 (if (find 'vacietis.c:double tokens)
		     'vacietis.c:long-double 
		     (if (find 'vacietis.c:unsigned tokens)
			 'vacietis.c:unsigned-long
			 'vacietis.c:long))))
	    ((find 'vacietis.c:signed tokens)
	     'vacietis.c:int)
	    ((find 'vacietis.c:unsigned tokens)
	     'vacietis.c:unsigned-int)
	    ((find 'vacietis.c:float tokens)
	     'vacietis.c:float)
	    ((find 'vacietis.c:double tokens)
	     'vacietis.c:double)
	    (t
					;	     (format t  "~&tok:~s" (car tokens))
	     (car (remove 'vacietis.c:* tokens))))
					;   #+nil
    (let ((derefs (count 'vacietis.c:* tokens)))
					;    #+nil
      (dotimes (x derefs)
	(setf token (list :pointer token))))
    token
    )) ;;only one token so use it



(defun read-base-type (token)
  (dbg "read-base-type: ~S~%" token)
  (let ((tokens (list token))
	(back-up nil)
	(outchar nil))
    (loop while
					;(type-qualifier? token)
	 (not (let ((next-char (peek-char t %in)))
					;;;		(print next-char)
		(flet ((test (char)
			 (if (char= next-char char)
			     (progn
			       (setf outchar char)
			       char)
			     nil)))
		  (or (test #\()
		      (test #\;)
		      (test #\{)
		      (test #\,))))) ;;;went too far
       do
	 (dbg "type qualifier token: ~S~%" token)
	 (setf back-up (file-position %in))
	 (setf token (next-exp))
	 (push token tokens))
;;    (format t "~&tokens ~s" tokens)
    (pop tokens)
    (map nil
	 (lambda (token)
	   (case token
	     (vacietis.c:extern
	      (setq *is-extern* t))
	     (vacietis.c:inline
	      (setq *is-inline* t))
	     (vacietis.c:const
	      (setq *is-const* t))	   
	     (vacietis.c:unsigned
	      (setq *is-unsigned* t))))
	 tokens)
    (file-position %in back-up)
    #+nil
    (awhen (gethash token (compiler-state-typedefs *compiler-state*))
      (setf token it))
    (cond ((eq token 'vacietis.c:enum)
	   (let ((name (next-exp))
					;		 (huh (next-exp))
		 )
					;	     (print token)
					;	     (print name)
	     (values
	      name
	      t
	      'enum)
					;	     (values (make-enum-type :name huh) t)
	     ))
	  ((find 'vacietis.c:struct tokens)
					;	   (print "ADfasdafs")
	   (dbg "  -> struct~%")
	   (if (eql #\{ (peek-char t %in))
	       (progn
		 (c-read-char)
		 (values (list
			  tokens
			  (read-struct-decl-body; (make-struct-type)
			   ))
			 t
			 'struct
			 ))
	       (progn
					;		 (print "ni")
		 (let ((name (next-exp)))
					;		   (print "234234")

		   (dbg "  -> struct name: ~S~%" name)
		   (values
		    #+nil
		    (or (gethash name (compiler-state-structs *compiler-state*)))
		    (list tokens name)
		    (if (char= outchar #\;)
			nil
			t)
		    'struct)))))
	  (t
	   (mehtype tokens)
	   #+nil
	   (values
	    (read-error "Unexpected parser error: unknown type ~A" token)
	    nil)))))

(defun read-struct-decl-body (;struct-type
			      )
 ; (declare (ignore struct-type))
;  (print 24234234234)
  (let (;(slot-index 0)
   ;     (*in-struct* t)
	acc)
    (loop for c = (next-char) until (eql #\} c) do
;	 (print c)
	 (let ((exp (read-c-exp c)))
	   (let ((base-type (read-base-type exp)))
	     (let ((next (next-exp)))
	       (let ((infix (read-infix-exp next)))
		 (prog1 (push (list base-type infix) acc)
		   #+nil
		   (when next
		     (multiple-value-bind (slot-name slot-type)
			 (process-variable-declaration infix
						       base-type)
		       (dbg "struct-type: ~S slot-name ~A i: ~D~%" struct-type slot-name slot-index)
					;    #+nil
		       (setf (gethash (format nil "~A.~A" (slot-value struct-type 'name) slot-name)
				      (compiler-state-accessors *compiler-state*))
			     (if *use-alien-types* slot-name slot-index)
			     (struct-type-slot-names struct-type)
			     (append (struct-type-slot-names struct-type)
				     (list slot-name))
			     (struct-type-slots struct-type)
			     (append (struct-type-slots struct-type)
				     (list slot-type)))
		       (incf slot-index)))))))))
    acc)
;  (print "lolz")
  )

(defun read-c-identifier-list (&optional c)
  (declare (ignore c))
  (map 'list (lambda (v)
               (if (= 1 (length v))
                   (aref v 0)
                   v))
       (c-read-delimited-list #\; #\,)))

(defun read-struct (struct-type &optional for-typedef)
  (acase (next-char)
    (#\{ (let ((body (read-struct-decl-body; struct-type
		      )))
	   #+nil
	   (awhen (struct-type-name struct-type)
	     (setf (gethash it (compiler-state-structs *compiler-state*))
		   struct-type))
	   (list body
		 (let ((c (next-char)))
		   (if (eql #\; c)
		       t
		       (progn (c-unread-char c)
			      (if for-typedef
				  (read-c-identifier-list c)
				  (read-variable-declarations #() struct-type))))))))
    (#\; t) ;; forward declaration
    (t   (if for-typedef
             (progn (c-unread-char it)
                    (read-c-identifier-list it))
             (read-variable-declarations (vector (read-c-exp it))
                                         struct-type)))))


(defun read-declaration (token)
  (cond
					;   #+nil
    ((eq 'vacietis.c:typedef token)
     (let* ((*is-unsigned* nil)
	    (*is-extern* nil)
	    (*is-const* nil)
	    (*is-unsigned* nil))
					;	   (print "yolo")
       (let ((next (next-exp)))
					;	     (print next)
	 (multiple-value-bind  (base-type is-decl other-type)
	     (read-base-type next)
	   (declare (ignorable is-decl))
	   ;;	       (print "baggins")
	       ;;;read-typedef
	   (dbg "read-typedef: ~S~%" base-type)
	   (cond ((eq 'struct other-type) ;;;(struct-type-p base-type)
		  (let ((names (read-struct base-type t)))
;;		   (print "@#$@#$@#$@")
		    (dbg "typedef read-struct names: ~S~%" names)
		    #+nil
		    (dolist (name names)
		      (when (symbolp name) ;; XXX handle pointer and array typedefs
			(setf (gethash name (compiler-state-typedefs *compiler-state*)) base-type)))
		    #+nil
		    (list 'struct
			  names)
		    base-type
					;t
		    ))
		 ;;new
		 ((eq 'enum other-type)
		  (list
		   'enum-???
		   (c-read-delimited-list #\; #\,)
		   base-type
		   ))
		 (t
	;;	  (print "@@#${}$@}@#${")
		  (let* ((token (next-exp))
			 )
		    ;;;;;;;FIXME:: multiple typedef names
;;		    (print (next-exp))
;;		    (print 2342342)
	;	    (read-infix-exp token)
					;	   (print wot)
		    (list
		     'typedef
		     token
		     base-type)
		    #+nil
		    (multiple-value-bind (name type)
			(process-variable-declaration wot base-type)
		      (declare (ignorable name type))
		      #+nil
		      (setf (gethash name (compiler-state-typedefs *compiler-state*)) type)
					;t
		      ))))))))
    (t ;(c-type? token)
     (let ((typedef?
	    (eq 'vacietis.c:typedef token)))
       (let* ((*is-inline* nil)
	      (*is-extern* nil)
	      (*is-const* nil)
	      (*is-unsigned* nil))
	 (multiple-value-bind (base-type is-decl other-type)
	     (read-base-type
	      (if typedef?
		  (next-exp)
		  token))
;;	   (print (list base-type is-decl other-type))
	   (dbg "read-declaration base-type: ~S~%" base-type)
	   ;;	   (format t "~&read-declaration base-type: ~S~% ~S~% ~S~%" base-type token is-decl)
	   (if (char= (peek-char nil %in) #\()
	       (read-c-exp (next-char))
	       (if is-decl
		   (case other-type
		     ((struct)
		      (read-struct base-type typedef?))
		     ((enum)
		      (prog1
			  (list 'enum base-type
				(next-exp)
					;(read-enum-decl)
				)
			(next-exp) ;;toss the semicolon ;
			)))
		   #+nil
		   (cond ((struct-type-p base-type)
			  (read-struct base-type))
			 ((enum-type-p base-type)
			  (read-enum-decl)))
		   (progn
		     ;;		     (print "fuckme")
					;		     (print base-type)
		     (read-var-or-function-declaration base-type))))))))))

(defun read-labeled-statement (token)
  (when (eql #\: (peek-char t %in))
    (next-char)
    (values (read-c-statement (next-char)) token)))

;#+nil
(defun read-infix-exp (next-token)
  (let ((exp (make-buffer)))
    (vector-push-extend next-token exp)
    (loop for c = (next-char nil)
       until (or (eql c #\;) (null c))
       do (vector-push-extend (read-c-exp c) exp))
    exp
    #+nil
    (parse-infix exp)))

(defun %read-c-statement (token)
  (multiple-value-bind (statement label) (read-labeled-statement token)
    (if label
	(values statement label)
	(progn
					;	  (print "a fuck")
	  (let ((foobar (read-declaration token)))
					;	    (print "fuck 7")
	    (if foobar
		(if (eq t foobar)
		    (values)
		    foobar)
		(or (read-control-flow-statement token)
		    (read-infix-exp token))))))))

(defun read-c-statement (c)
					;  (print (file-position %in))
					;  (print c)
  (case c
    (#\# (read-c-macro %in c))
    (#\; (values))
    (t
					;     (print "a")
     (let ((wot (read-c-exp c)))
					;       (print "b")
       (%read-c-statement wot)))))

(defun read-c-identifier (c)
  ;; assume inverted readtable (need to fix for case-preserving lisps)
  (let* ((raw-name (concatenate
                    'string (string c)
                    (slurp-while (lambda (c)
                                   (or (eql c #\_)
				       (alphanumericp c))))))
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
  (c-read-delimited-list #\{ #\,))

(defun read-c-exp (c)
  (or (match-longest-op c)
      (cond ((digit-char-p c)
	     (read-c-number c))
	    ((or (eql c #\_)
		 (alpha-char-p c))
	     (read-c-identifier c))
	    (t
	     (case c
	       (#\" (read-c-string %in c))
	       (#\' (read-character-constant %in c))
	       (#\( (read-exps-until (lambda (c) (eql #\)
						      c))))
	       (#\{ (read-vector-literal)) ;; decl only
	       (#\[ (list 'vacietis.c:[]
			  (read-exps-until (lambda (c) (eql #\] c))))))))))

;;; readtable

(defun read-c-toplevel (%in c)
  (read-c-statement c))

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

(defparameter *fun* (function identity))
(defun treeify (x)
  (if (typep x 'sequence)
      (map 'list #'treeify
	   x)
      (funcall *fun* x)))


(defun wow (*c-file* &optional (*compiler-state* (make-compiler-state))
			)
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
   (format t "~&~%;;;;~a~%~%" (pathname-name *c-file*))
   (with-open-file (stream *c-file*)
     (let ((%in stream))
       (let ((eof (list nil)))
	 (let* ((package
		 (find-package '"VACIETIS.C"))
		(*fun* (lambda (x)
			 (if (and (symbolp x)
				  (eq package
				      (symbol-package x)))
			     (symbol-name x)
			     x))))
	   (loop
	      (let ((value (read stream nil eof)))
		(if (eq value eof)
		    (return)
		    (let ((value (treeify value)))
		      (format t
			      "~&::~s~%" value))
		    )))))))))

(defparameter *directory*
  (format nil
	  "/home/imac/install/llvm/~A/"
					;#+nil
	  "3.8.0/"
	  #+nil
	  "6.0.0/"
	  ))
(defparameter *directory-transforms*
  (merge-pathnames "Transforms/" *directory*))
(defun wot (&optional (directory *directory*))
  (let ((c-include-files-dir (merge-pathnames "src/include/llvm-c/" directory)))
    (let ((*directory-transforms* (merge-pathnames "Transforms/" c-include-files-dir)))
      (labels ((dump (file)
		 (wow file))
	       (stuff (files)
		 (dolist (file files)
		   (when (string= "h"
				  (pathname-type file))
		     (dump file)))))
	(stuff (uiop:directory-files c-include-files-dir))
	(stuff (uiop:directory-files *directory-transforms*))))))
