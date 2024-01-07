;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun compile-to-class (class-name class-source-code)
  (let ((string-builder (java:jnew "java.lang.StringBuilder")))

    (java:jcall "append" string-builder class-source-code)
    (java:jcall
     "compile"
     (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler")
     class-name
     (java:jcall "toString" string-builder))))
