;;;; package.lisp

(defpackage #:abcl-memory-compiler
  (:use #:cl)
  (:export #:compile-to-class
           #:compile-to-multiple-classes
           #:quick-compile-to-class
           #:quick-compile-to-multiple-classes))
