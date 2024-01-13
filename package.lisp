;;;; package.lisp

(defpackage #:abcl-memory-compiler
  (:use #:cl)
  (:export #:compile-to-class
           #:compile-to-multiple-classes))
