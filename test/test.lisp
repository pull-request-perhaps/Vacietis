;;; The contents of this file are released into the public domain.

(in-package #:vacietis.test)
(in-readtable vacietis)

(def-suite vacietis-reader)
(def-suite basic-tests)
(def-suite program-tests)
(def-suite pointer-tests)

(defun run-reader-tests ()
  (format t "Running reader tests:~&")
  (run! 'vacietis-reader))

(defun run-pointer-tests ()
  (format t "Running pointer tests:~&")
  (run! 'pointer-tests))

(defun run-basic-tests ()
  (format t "Running basic tests:~&")
  (run! 'basic-tests))

(defun run-program-tests ()
  (format t "Running program tests:~&")
  (run! 'program-tests))

(defun run-tests ()
  (run-reader-tests)
  (run-pointer-tests)
  (run-basic-tests)
  (run-program-tests))

(defmacro reader-test (name input &rest s-exps)
  `(test ,name
     (is (equalp '(progn ,@s-exps)
                 (vacietis::cstr-noeval ,input)))))

(defun do-with-temp-c-package (name thunk)
  (let ((test-package (make-package
                       (gensym (format nil "VACIETIS.TEST.~A" name))
                       :use ())))
    (unwind-protect
         (let ((*package* test-package))
           (funcall thunk))
      (delete-package test-package))))

(defmacro eval-test (name input result)
  `(test ,name
     (is (equalp ,(if (stringp result)
                      `(string-to-char* ,result)
                      ;;`(vacietis.c:mkptr& (aref (string-to-char* ,result) 0))
                      result)
                 (do-with-temp-c-package ',name
                   (lambda ()
                     (eval (vacietis::cstr ,input))))))))

(defvar *test-dir*
  (asdf:system-relative-pathname :vacietis "test/"))

(defmacro test-dir ()
  *test-dir*
  #+nil
  (directory-namestring (or *load-truename* *compile-file-truename*)))

(defmacro program-test (name &key return-code input output)
  `(test ,name
     (do-with-temp-c-package ',name
       (lambda ()
         (load-c-file
          (merge-pathnames
           (format nil "programs/~(~A~)/main.c" ',name)
           (test-dir)))
         (let* ((test-output-stream (when ,output
                                      (make-instance 'fast-io:fast-output-stream
                                                     :buffer (fast-io:make-output-buffer))
                                      #+nil
                                      (make-string-output-stream)))
                (result (run-c-program
                         *package*
                         :stdin (when ,input
                                  (make-instance 'fast-io:fast-input-stream
                                                 :buffer (fast-io:make-input-buffer
                                                          :vector (vacietis:string-to-unsigned-char* ,input)))
                                  #+nil
                                  (make-string-input-stream ,input))
                         :stdout test-output-stream)))
           (declare (ignorable result))
           (when ,return-code
             (is (equal ,return-code result)))
           (when ,output
             (is (equal (vacietis:char*-to-string (vacietis:string-to-unsigned-char* ,output))
                        (vacietis:char*-to-string (fast-io:finish-output-stream test-output-stream))
                        #+nil
                        (get-output-stream-string
                         test-output-stream)))))))))
