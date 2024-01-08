;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun get-classpath-jars (current-classloader)
  (cdar (java:dump-classpath current-classloader)))


(defun compile-to-class (class-name class-source-code)
  (let ((string-builder (java:jnew "java.lang.StringBuilder"))
        (memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler"))
        (current-classloader (java:get-current-classloader)))

    (java:jcall "append" string-builder class-source-code)

    (java:jcall "useParentClassLoader" memory-compiler current-classloader)

    (java:jcall "useOptions" memory-compiler
                (java:jarray-from-list
                 (list "-classpath"
                       (format nil "~{~a~^: ~}" (get-classpath-jars current-classloader))
                       "-Xlint:none")))
    (java:jcall
     "compile"
     memory-compiler
     class-name
     (java:jcall "toString" string-builder))))
