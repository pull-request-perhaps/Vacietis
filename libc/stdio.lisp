(in-package #:vacietis.libc.stdio.h)
(in-readtable vacietis)

(define EOF -1)

;;; FILEs/streams

(defclass FILE ()
  ((stream   :initarg  :stream  :accessor fd-stream)
   (feof     :initform 0        :accessor feof)
   (ferror   :initform 0        :accessor ferror)
   (unread   :initform nil      :accessor unread)
   (tmp-file :initform nil      :accessor tmp-file)))

(defvar stdin)
(defvar stdout)
(defvar stderr)

(defun init-stdio ()
  (setq stdin
        (make-instance 'FILE
                       :stream
                       #+sbcl (sb-sys:make-fd-stream 0 :element-type '(unsigned-byte 8))
                       #-sbcl
                       (open "/dev/stdin"
                             :direction :input
                             :if-does-not-exist nil
                             :element-type '(unsigned-byte 8))))
  (setq stdout (make-instance 'FILE
                              :stream (open "/dev/stdout"
                                            :direction :output
                                            :if-exists :append
                                            :element-type '(unsigned-byte 8))))
  (setq stderr (make-instance 'FILE
                              :stream (open "/dev/stderr"
                                            :direction :output
                                            :if-exists :append
                                            :element-type '(unsigned-byte 8)))))

(vacietis.libc:add-runtime-init-hook #'init-stdio)

(defun/1 clearerr (fd)
  (setf (feof fd)   0
        (ferror fd) 0))

;;; file operations

;; have to do something about EEXIST
(defun open-stream (filename mode)
  (let* ((m (char*-to-string mode))
         (opts (cond ((string= m "r") '(:direction :input))
                     ((string= m "w") '(:direction :output :if-exists :overwrite
                                        :if-does-not-exist :create))
                     ((string= m "a") '(:direction :output :if-exists :append
                                        :if-does-not-exist :create))
                     ((string= m "r+") '(:direction :io))
                     ((string= m "w+") '(:direction :io :if-exists :overwrite
                                        :if-does-not-exist :create))
                     ((string= m "a+") '(:direction :io :if-exists :append
                                         :if-does-not-exist :create)))))
    (apply #'open (char*-to-string filename) opts)))

(defun/1 fopen (filename mode)
  (handler-case (make-instance 'FILE :stream (open-stream filename mode))
    (file-error ()
      (setf errno ENOENT)
      NULL)
    (error ()
      NULL)))

(defun/1 fflush (fd)
  (unless (eql fd NULL)
    (finish-output (fd-stream fd)))
  0)

(defun/1 fclose (fd)
  (close (fd-stream fd))
  (when (tmp-file fd)
    (delete-file (tmp-file fd))
    (setf (tmp-file fd) nil))
  0)

(defun/1 freopen (filename mode fd)
  (handler-case (progn (fclose fd)
                       (clearerr fd)
                       (setf (fd-stream fd) (open-stream filename mode)))
    (error ()
      (setf errno EIO)
      NULL)))

(defun/1 remove (filename)
  (handler-case (progn (delete-file (char*-to-string filename))
                       0)
    (file-error () 1)))

(defun/1 rename (oldname newname)
  (handler-case (progn (rename-file oldname newname)
                       0)
    (file-error () 1)))

(defun/1 tmpfile ()
  (let ((path (merge-pathnames (symbol-name (gensym "vac_tmp_c_file"))
                               "/tmp/")))
    (if (open path :direction :probe)
        (tmpfile) ;; try again
        (let ((fd (fopen (string-to-char* (namestring path))
                         (string-to-char* "w+")))) ;; should be wb+
          (unless (eql fd NULL)
            (setf (tmp-file fd) path))
          ;; also need to make sure tmp files are deleted on exit
          ;; good idea to attach finalizers to the tmp files' streams too
          fd))))

(defun/1 tmpnam (str)
  (let ((newname (string-to-char* (symbol-name (gensym)))))
   (if (eql str NULL)
       newname
       (progn (replace str newname :end1 (length newname))
              str))))

;;; character I/O

(defun c-char-code (c)
  (if (characterp c)
      (char-code c)
      (if (> c 127)
          (- (- c 127))
          c)))

(defun c-read-char (stream)
  (read-byte stream))

(defun %fgetc (fd)
  (let ((it (unread fd)))
    (if it
        (progn (setf (unread fd) nil)
               it)
        (c-char-code (c-read-char (fd-stream fd))))))

(defun/1 fgetc (fd)
  (handler-case
      (%fgetc fd)
    (end-of-file ()
      (setf (feof fd) 1)
      EOF)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun/1 getc (fd)
  (fgetc fd))

(defun/1 getchar ()
  (getc stdin))

(defun c-code-char (c)
  (if (characterp c)
      (char-code c)
      (if (< c 0)
          (+ 127 (- c))
          c)))

(defun buffer-write-byte (byte buffer)
  (vector-push-extend byte buffer))

(defun c-write-char (c stream)
  (let ((byte (if (characterp c)
                  (char-code c)
                  c)))
    (if (streamp stream)
        (progn
          (write-byte byte stream)
          (when (= 10 byte)
            (force-output stream)))
        (buffer-write-byte byte stream))))

(defun/1 fputc (c fd)
  (handler-case (progn (c-write-char (c-code-char c) (fd-stream fd))
                       (when (streamp (fd-stream fd))
                         (force-output (fd-stream fd)))
                       c)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun/1 putc (c fd)
  (fputc c fd))

(defun/1 putchar (c)
  (fputc c stdout))

(defun fgets-is-dumb (str n fd replace-newline?)
  (handler-case
      (progn
        (loop for i from 0 below (1- n)
           for x = (%fgetc fd)
           do (progn (setf (aref str i) (c-char-code x))
                     (when (eql x (char-code #\Newline))
                       (unless replace-newline? (incf i))
                       (loop-finish)))
           finally (setf (aref str i) 0))
        str)
    (end-of-file ()
      (setf (feof fd) 1)
      NULL)
    (error ()
      (setf (ferror fd) EIO)
      NULL)))

(defun/1 fgets (str n fd)
  (fgets-is-dumb str n fd nil))

(defun/1 gets (str)
  (fgets-is-dumb str most-positive-fixnum stdin t))

(defun c-write-string (str stream)
  (let ((seq (vacietis::ensure-unsigned-sequence str)))
    (write-sequence seq stream :end (position 0 seq))))

(defun/1 fputs (str fd)
  (handler-case (progn (c-write-string str (fd-stream fd))
                       0)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun/1 puts (str)
  (when (eql EOF (fputs str stdout))
    (return-from puts EOF))
  (when (eql EOF (fputc #\Newline stdout))
    (return-from puts EOF))
  0)

(defun/1 ungetc (c fd)
  (handler-case (progn (setf (unread fd) c)
                       c)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

;;; fread/fwrite, only work for byte arrays for now

(defun/1 fread (mem element_size count fd)
  ;;(format t "fread ~S ~S ~S ~S~%" mem element_size count fd)
  (handler-case
      (let* ((max (* element_size count))
             (mem (vacietis::ensure-memptr mem))
             (type (vacietis::memptr-type mem)))
        ;;(format t "fread type: ~S~%" type)
        (labels ((read-bytes ()
                   (loop for i from 0 below max
                      for x = (%fgetc fd)
                      do (setf (aref (memptr-mem mem) i) (c-char-code x))
                      finally (return i)))
                 (read-ints (size-in-bytes)
                   (let ((buffer (make-array size-in-bytes :element-type '(unsigned-byte 8)))
                         (memi 0))
                     (loop for i from 0 below max
                        for x = (%fgetc fd) for bufi = (mod i size-in-bytes)
                        do (progn
                             (setf (aref buffer bufi) x)
                             (setf (ldb (byte 8 (* 8 bufi)) (aref (memptr-mem mem) memi)) x)
                             (when (= bufi (1- size-in-bytes))
                               #+nil
                               (format t (format nil "~~A -> ~~~D,'0X~~%" (* size-in-bytes 2))
                                       (with-output-to-string (s)
                                         (dotimes (z size-in-bytes)
                                           (format s "~2,'0X" (aref buffer z))))
                                       (aref (memptr-mem mem) memi))
                               (incf memi)))
                        finally (return i)))))
          (let* ((n-read (cond
                           ((or (eq (car type) 'signed-byte) (eq (car type) 'unsigned-byte))
                            (if (eql 8 (cadr type))
                                (read-bytes)
                                (read-ints (vacietis.c:integer/ (cadr type) 8))))
                           (t
                            (error (format nil "unsupported type ~S" type))))))
            (when (< n-read max)
              (setf (feof fd) 1))
            (vacietis.c:integer/ n-read element_size))))
    (error (x)
      ;;(format t "fread io error: ~S~%" x)
      (setf (ferror fd) EIO)
      0)))

(defun/1 fwrite (mem element_size count fd)
  (handler-case
      (let* ((mem (vacietis::ensure-memptr mem))
             (start (memptr-ptr mem))
             (seq (vacietis::ensure-unsigned-sequence mem)))
        (write-sequence seq (fd-stream fd)
                        :start start :end (+ start (* element_size count)))
        (force-output (fd-stream fd))
        count)
    (error ()
      (setf (ferror fd) EIO)
      0)))

;;; file positioning

(define SEEK_SET 0)
(define SEEK_CUR 1)
(define SEEK_END 2)

(defun/1 fseek (fd offset origin) ;; dumbest function in stdio
  (handler-case
      (let ((stream (fd-stream fd)))
        (file-position stream (case origin
                                (0 offset)
                                (1 (+ offset (file-position stream)))
                                (2 (+ offset (file-length stream)))))
        (setf (feof fd) 0)
        0)
    (error ()
      (setf (ferror fd) ESPIPE) ;; is this the right error code?
      1)))

(defun/1 ftell (fd)
  (or (file-position (fd-stream fd)) -1))

(defun/1 rewind (fd)
  (fseek fd 0 0)
  (clearerr fd))

(defun/1 fgetpos (fd pos_ptr)
  (let ((pos (file-position (fd-stream fd))))
    (if pos
        (progn (setf (deref* pos_ptr) pos)
               0)
        (progn (setf errno ENOTTY)
               1))))

(defun/1 fsetpos (fd pos_ptr)
  (handler-case (progn (if (file-position (fd-stream fd) (deref* pos_ptr))
                           0
                           (progn (setf errno ESPIPE)
                                  1)))
    (error ()
      (setf errno ESPIPE) ;; is this the right code?
      1)))

;;; printf

;; adapted from ZetaC
(defmacro with-padding (count &body body) ;; variable capture ahoy
  `(progn
     (when right-justify?
       (loop repeat ,count do (c-write-char pad-char stream)))
     ,@body
     (unless right-justify?
       (loop repeat ,count do (c-write-char pad-char stream)))))

(defun zclib>read-decimal-from-string (str idx)
  "Reads a decimal value out of a string, stopping at the first
   non-digit. Returns the value read and the next index in the
   string."
  (let ((positive (case (c-code-char (aref str idx))
                    (#\+ (incf idx) t)
                    (#\- (incf idx) nil)
                    (t t))))
    (do ((ch (aref str idx) (aref str (incf idx)))
         (val 0 (+ (* val 10.) (- ch (char-code #\0)))))
        ((not (and (>= ch (char-code #\0)) (<= ch (char-code #\9))))
         (values (if positive val (- val)) idx)))))

(defun zclib>print-integer (val width precision pad-char right-justify?
			    alternate-form? uppercase-hex? always+- spacep pbase
			    stream)
  (unless (and (zerop val) (zerop precision))	; If PRECISION=0, don't print '0'
    (let* ((sign       (cond ((minusp val) (setf val (- val)) "-")
                             (always+-                        "+")
                             (spacep                          " ")
                             (t                                "")))
	   (buffer     (format nil "~A~VR"
                               (cond ((and alternate-form? (= pbase 8))
                                      "0")
                                     ((and alternate-form? (= pbase 16))
                                      (if uppercase-hex? "0X" "0x"))
                                     (t ""))
                               pbase val))
	   (val-len    (+ (length buffer) (length sign)))
	   (leading-0s (max 0 (- precision val-len))))
      (unless uppercase-hex?
        (string-downcase buffer))
      (with-padding (- width (+ val-len leading-0s))
        (c-write-string sign stream)
        (loop repeat leading-0s			; This is how ANSI says to do this
              do (c-write-char #\0 stream))
        (c-write-string buffer stream)))))

(defun zclib>print-flonum-1 (val precision uppercase-E-format? e-format)
  "Returns the printed flonum as a string. VAL is assumed to be non-negative."
  (if (or (eql e-format #\e)		; We must go to Exx format.
          (and (eql e-format #\g)
               ;; PRECISION tells %g when to use Exx format.
               (or (> val (expt 10 (1+ precision)))
                   (< val 1.0e-4))))
      (format nil
              (format nil "~~,~d,2,,,,'~cE"
                      precision (if uppercase-E-format? #\E #\e))
              val)
      (format nil (format nil "~~,~dF" precision) val)))

(defun zclib>print-flonum (val width precision pad-char right-justify?
                           uppercase-E-format? always+- spacep
			   conv-char stream)
  "CONV-CHAR should be one of #\e, #\f, #\g"
  (let* ((negative? (minusp val))
	 (buffer    (zclib>print-flonum-1 (abs val) precision
                                          uppercase-E-format? conv-char))
         (val-len   (+ (length buffer)
                       (if (or negative? always+- spacep) 1 0))))
    (with-padding (- width val-len)
      (cond (negative? (c-write-char #\-     stream))
            (always+-  (c-write-char #\+     stream))
            (spacep    (c-write-char #\Space stream)))
      (c-write-string buffer stream))))

(defun/1 fprintf (fd fmt &rest args)
  "Prints ARGS to FD according to FMT.
   Characters in FMT are just copied to the output, except for %, which introduces
   a directive.  A directive has the following syntax:
     %[-][0][<width>][.<precision>][l]<conv>
   <conv> can be one of the following:
     d o x     The integer <arg> is printed in decimal, octal, or hex respectively.
     f         The float or double <arg> is printed in the style `[-]ddd.ddd' with
               <precision> digits after the decimal point (default 6).
     e	       The float or double <arg> is printed in the style `[-]d.ddddde[-]dd'
               with <precision> digits after the decimal point (default 6).
     g         The float or double <arg> is printed in f or e style, as appropriate
               for its magnitude.
     s	       The string <arg> is printed; if <precision> is specified, it is the
	       maximum number of characters to print.
     c	       The character <arg> is printed.  NULs are ignored.
   If a minus sign appears before <width>, the value is left justified in the
   field; if a zero appears before <width>, padding will be done with zeros instead
   of blanks.  An `l' before <conv> is ignored, as is the case of <conv>."
  (let* ((fmt (vacietis::ensure-memptr fmt))
         ;;(__ (format t "fmt memptr: ~S~%" fmt))
         (fmt-array (memptr-mem fmt))
         (stream    (fd-stream fd)))
    (do ((fmt-index (memptr-ptr fmt) (1+ fmt-index)))
        ((= (aref fmt-array fmt-index) 0) 0)
      (let ((ch (aref fmt-array fmt-index)))
        (if (eql ch (char-code #\%))
            (let ((next-idx (incf fmt-index))
                  right-justify? pad-char always+- space-flag alternate-form
                  width precision uppercase)

              ;; First we look for flags, assuming their order if present
              (if (= (char-code #\-) (aref fmt-array next-idx))
                  (incf next-idx)         ; Skip '-'
                  (setf right-justify? t))
             (if (= (char-code #\0) (aref fmt-array next-idx))
                 ;; Here is where UNIX and H&S expect the '0' flag. See ** below.
                 (progn (setq pad-char #\0) (incf next-idx)) ; Skip '0'
                 (setq pad-char #\Space))
             (when (= (char-code #\+) (aref fmt-array next-idx))
               (setf always+- t) (incf next-idx)) ; Skip '+'
             (when (= (char-code #\Space) (aref fmt-array next-idx))
               (setf space-flag #\Space) (incf next-idx)) ; Skip ' '
             (when (= (char-code #\#) (aref fmt-array next-idx))
               (setf alternate-form t) (incf next-idx))	; Skip '#'
             (when (= (char-code #\l) (aref fmt-array next-idx))
               (incf next-idx))	; Skip 'l'

             ;; Get width, if present
             (if (= (char-code #\*) (aref fmt-array next-idx))
                 (progn
                   (incf next-idx)          ; Skip over the '*'
                   (setq width (pop args))) ; Consume an arg for the width
                 (progn
                   (when (= (char-code #\0) (aref fmt-array next-idx))
                     ;; Here is where ANSI expects the '0' flag. See ** above.
                     (incf next-idx) (setq pad-char #\0)) ; Skip '0'
                   (multiple-value-setq (width next-idx)
                     ;; If width is absent, 0 is 1st value returned
                     (zclib>read-decimal-from-string fmt-array next-idx))))

             (when (minusp width)
               (setf right-justify? nil ; Per ANSI spec
                     width          (abs width)))

             ;; Get precision, if present
             (when (= (char-code #\.) (aref fmt-array next-idx))
               (incf next-idx)          ; Skip over '.'
               (if (= (char-code #\*) (aref fmt-array next-idx))
                   (progn
                     (incf next-idx)              ; Skip over '*'
                     (setf precision (pop args))) ; get arg for the precision
                   (multiple-value-setq (precision next-idx)
                     ;; If width is absent, 0 is 1st value returned
                     (zclib>read-decimal-from-string fmt-array next-idx))))

             (when (and precision (minusp precision))
               (setf precision nil))    ; Per ANSI spec
             (when (find (c-code-char (aref fmt-array next-idx)) "lLh")
               (incf next-idx))         ; Discard long/short info

             (let ((char (code-char (aref fmt-array next-idx))))
               (setf fmt-index next-idx)

               (when (upper-case-p char)
                 (setq uppercase t
                       ;; No int/long distinction - let uppercase %D, %U, etc. thru
                       char      (char-downcase char)))

               (case char
                 ((#\d #\i #\o #\x #\u)
                  (assert (integerp (car args)))
                  (zclib>print-integer (pop args)
                                       (or width 0)
                                       (or precision 1)
                                       pad-char
                                       right-justify?
                                       alternate-form
                                       uppercase
                                       always+-
                                       space-flag
                                       (case char
                                         ((#\d #\i #\u) 10)
                                         (#\o            8)
                                         (#\x           16))
                                       stream))
                 ((#\e #\f #\g)
                  (zclib>print-flonum (float (pop args))
                                      (or width 0)
                                      (or precision 6)
                                      pad-char
                                      right-justify?
                                      uppercase
                                      always+-
                                      space-flag
                                      ch
                                      stream))
                 (#\c
                  (with-padding (1- width)
                    (unless (zerop (car args))
                      (c-write-char (c-code-char (pop args)) stream))))
                 (#\s
                  (let* ((string (vacietis::ensure-memptr (pop args)))
                         (length (min (or precision most-positive-fixnum)
                                      (vacietis.libc.string.h:strlen string))))
                    (with-padding (- width length)
                      (let ((str   (memptr-mem string))
                            (start (memptr-ptr string)))
                        (loop for i from start below (+ start length) do
                             (c-write-char (c-code-char (aref str i)) stream))))))
                 (otherwise
                  (c-write-char char stream)))))
            (c-write-char (c-code-char ch) stream))))))

(defun/1 printf (fmt &rest args)
  ;;(format t "printf: ~S~%" (vacietis:char*-to-string fmt))
  (apply #'fprintf stdout fmt args))

(defun/1 sprintf (str fmt &rest args)
  (let ((str (vacietis::ensure-memptr str)))
    (replace
     (memptr-mem str)
     (let ((buffer (make-array 10 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
       (apply #'fprintf (make-instance 'FILE :stream buffer) fmt args)
       buffer)
     :start1 (memptr-ptr str))
    str))

(defun/1 snprintf (string max-length fmt &rest args)
  (error "NOT IMPLEMENTED YET"))

(defun/1 perror (str)
  (if (or (eql NULL str)
          (eql 0 (aref (memptr-mem str) (memptr-ptr str))))
      (fprintf stderr (string-to-char* "%s\\n") (strerror errno))
      (fprintf stderr (string-to-char* "%s: %s\\n") str (strerror errno))))

;;; scanf

(load-libc-file "scanf.c" #.(libc-dir))

;;; things that have no effect

(defun/1 setvbuf (fd buf mode size)
  (declare (ignore fd buf mode size))
  0)

(defun/1 setbuf (fd buf)
  (declare (ignore fd buf))
  0)

(define FILENAME_MAX 1024)
(define FOPEN_MAX    1024)
(define BUFSIZ       512)
(define L_tmpnam     16)
(define TMP_MAX      1024)
(define _IOFBF       1)
(define _IOLBF       2)
(define _IONBF       3)
