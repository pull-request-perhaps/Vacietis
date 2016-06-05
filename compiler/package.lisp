(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables #:anaphora)
  (:export
   ;; readtables
   #:vacietis
   #:c-readtable

   ;; memory stuff
   #:size-of
   #:allocate-memory
   #:memptr-mem
   #:memptr-ptr
   #:copy-memptr

   ;; utilities
   #:string-to-char*
   #:string-to-unsigned-char*
   #:char*-to-string

   ;; runtime
   #:*compiler-state*
   #:make-compiler-state
   #:load-c-file
   #:run-c-program
   ))

(in-package #:vacietis)

(defparameter *debug* nil)
(defmacro dbg (&rest rest)
  `(when *debug*
     (format t ,@rest)))
(defparameter *verbose* nil)
(defmacro verbose (&rest rest)
  `(when *verbose*
     (format t ,@rest)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable vacietis ;; defreadtable isn't eval-whened, grrr
    (:merge :standard)
    (:case :invert)))

(in-readtable vacietis)

(defpackage #:vacietis.c
  (:use)
  (:export
   ;; operators
   #:=
   #:+=
   #:-=
   #:*=
   #:/=
   #:%=
   #:<<=
   #:>>=
   #:&=
   #:^=
   #:|\|=|
   #:?
   #:|:|
   #:|\|\||
   #:&&
   #:|\||
   #:^
   #:&
   #:==
   #:!=
   #:<
   #:>
   #:<=
   #:>=
   #:<<
   #:>>
   #:++
   #:--
   #:+
   #:-
   #:*
   #:/
   #:%
   #:!
   #:~
   #:->
   #:|.|
   #:?
   #:|:|
   #:|,|

   ;; keywords
   #:auto
   #:break
   #:case
   #:char
   #:const
   #:continue
   #:default
   #:do
   #:double
   #:else
   #:enum
   #:extern
   #:float
   #:for
   #:goto
   #:if
   #:inline
   #:int
   #:long
   #:register
   #:restrict
   #:return
   #:short
   #:signed
   #:sizeof
   #:static
   #:struct
   #:switch
   #:typedef
   #:union
   #:unsigned
   #:void
   #:volatile
   #:while
   #:_Bool
   #:_Complex
   #:_Imaginary

   ;; preprocessor
   #:define
   #:undef
   #:include
   #:if
   #:ifdef
   #:ifndef
   #:else
   #:endif
   #:line
   #:elif
   #:pragma
   #:error

   ;; stuff we define
   #:deref*
   #:mkptr&
   #:post--
   #:post++
   #:[]
   #:alien[]
   #:|...|

   ;; math
   #:truncl
   #:ceil
   ;; integer operations
   #:integer/
   #:integer/=
   ;; pointer operations
   #:ptr+
   #:ptr+=
   #:ptr-
   #:ptr-=
   #:ptr<
   #:ptr<=
   #:ptr>
   #:ptr>=
   #:ptr==
   #:ptr!=

   ;; unsigned types
   #:unsigned-char
   #:unsigned-short
   #:unsigned-int
   #:unsigned-long
   ))
