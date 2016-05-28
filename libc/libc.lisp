(in-package #:vacietis.libc)

(defvar *runtime-init-hooks* nil)

(defun add-runtime-init-hook (f)
  (push f *runtime-init-hooks*))

(defun runtime-init ()
  (dolist (f *runtime-init-hooks*)
    (funcall f)))
