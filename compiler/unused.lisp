#+nil
(defvar *local-var-types* nil)
#+nil
(defvar *local-variables* nil)

#+nil
(defun size-of (x)
  (or (type-size x)
      (type-size (gethash x (or *local-var-types*
                                (compiler-state-var-types *compiler-state*))))))

#+nil
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

#+nil
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

#+nil
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

#+nil
(defun struct-name-of-type (type)
  (cond
    ((pointer-to-p type)
     (struct-name-of-type (pointer-to-type type)))
    ((struct-type-p type)
     (struct-type-name type))
    (t
     nil)))

;;; infix

#+nil
(defvar *variable-declarations-base-type*)

#+nil
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

#+nil
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
#+nil
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

#+nil
(defun integer-type? (type)
  (member type
          '(vacietis.c:long vacietis.c:int vacietis.c:short vacietis.c:char
            vacietis.c:unsigned-long vacietis.c:unsigned-int vacietis.c:unsigned-short vacietis.c:unsigned-char)))


#+nil
(defun one-long-progn (body)
  (loop for x in body
     nconc (cond
             ((and (listp x) (eq 'progn (car x)))
              (one-long-progn (cdr x)))
             (t
              (list x)))))
#+nil
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
#+nil
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



#+nil
(defvar *alien-value-ctr* 0)
#+nil
(defvar *alien-value-table* (make-hash-table :weakness :value))

#+nil
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

#+nil
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

#+nil
(defun get-dimensions (name1 &optional dimensions)
  (dbg "get-dimensions name1: ~S~%" name1)
  (let ((dim1 (third name1)))
    (if (listp (second name1))
	(nconc dimensions (get-dimensions (second name1)) (list dim1))
	(nconc dimensions (list dim1)))))

#+nil
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

#+nil
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

#+nil
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
#+nil
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

;(defvar *in-struct* nil)
#+nil
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



;;; reader

#+nil
(defun cstr (str)
  (dbg "cstr: ~S~%" str)
  (with-input-from-string (s str)
    (let ((*compiler-state* (make-compiler-state))
          (*readtable*      c-readtable))
      (let ((body (cons 'progn (loop for it = (read s nil 'eof)
                                  while (not (eq it 'eof)) collect it))))
        (eval `(vac-progn/1 ,body))))))

#+nil
(defun cstr-noeval (str)
  (with-input-from-string (s str)
    (let ((*compiler-state* (make-compiler-state))
          (*readtable*      c-readtable))
      (let ((body (cons 'progn (loop for it = (read s nil 'eof)
                                  while (not (eq it 'eof)) collect it))))
        body))))

#+nil
(defun %load-c-file (*c-file* *compiler-state*)
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
    (load *c-file*)))

#+nil
(defun load-c-file (file)
  (%load-c-file file (make-compiler-state)))

(defun read-c-toplevel (%in c)
  (let* (;(*macro-stream* nil)
         (exp1           (read-c-statement c)))
    ;;(dbg "toplevel: ~S~%" exp1)
    exp1
    #+nil
    (if (and *macro-stream* (peek-char t *macro-stream* nil))
        (list* 'progn
               exp1
               (loop while (peek-char t *macro-stream* nil)
		  collect (read-c-statement (next-char))))
        (or exp1 (values)))))

(defun read-c-exp (c)
  (or (match-longest-op c)
      (cond ((digit-char-p c)
	     (read-c-number c))
	    ((or (eql c #\_)
		 (alpha-char-p c))
	     (let ((symbol (read-c-identifier c)))
	       symbol
	       ;;(dbg "~S -> symbol: ~S~%" c symbol)
	       #+nil
	       (when (eq t symbol)
		 (setq symbol '__c_t))
					;	       (print %in)
	       #+nil
	       (acond
		 #+nil
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
		 #+nil
		 ((gethash symbol (compiler-state-enums *compiler-state*))
		  ;;(dbg "returning it...~%")
		  it)
		 (t
		  symbol))))
	    (t
	     (case c
	       (#\" (read-c-string %in c))
	       (#\' (read-character-constant %in c))
	       (#\( (read-exps-until (lambda (c) (eql #\)
						      c))))
	       (#\{ (read-vector-literal)) ;; decl only
	       (#\[ (list 'vacietis.c:[]
			  (read-exps-until (lambda (c) (eql #\] c))))))))))


(defun read-vector-literal ()
  #+nil
  (make-vector-literal
   :elements ;(map 'list #'parse-infix)
)
  (c-read-delimited-list #\{ #\,)
		  )



#+nil
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

#+nil
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

#+nil
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

#+nil
(defmacro evaluate-arithmetic-expression (expr)
  `(vac-arithmetic-expression-override
    (let ((expanded-body (macroexpansion-of ,expr)))
      ;;(dbg "expanded-body: ~S~%" expanded-body)
      (eval expanded-body))))
#+nil
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

