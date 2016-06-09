(in-package #:vacietis)
(in-readtable vacietis)

(defvar *compiler-state*)

(defstruct compiler-state
  (pp        (make-hash-table))
  (typedefs  (make-hash-table))
  (structs   (make-hash-table))
  (accessors (make-hash-table :test #'equal))
  (enums     (make-hash-table))
  (functions (make-hash-table))
  (variables (make-hash-table))
  (var-types (make-hash-table)))
