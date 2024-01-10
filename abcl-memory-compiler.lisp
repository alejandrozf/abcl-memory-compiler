;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun get-classpath-jars (current-classloader)
  (cdar (java:dump-classpath current-classloader)))


(defun compile-to-class (class-name class-source-code
                         &optional
                           (classloader
                            (java:jnew "org.armedbear.lisp.JavaClassLoader"
                                       (java:jcall "getClassLoader"
                                                   (java:jclass "org.armedbear.lisp.LispObject")))))
  (let ((string-builder (java:jnew "java.lang.StringBuilder"))
        (memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler"))
        (current-classloader classloader))

    (java:jcall "append" string-builder class-source-code)

    (java:jcall "useParentClassLoader" memory-compiler current-classloader)

    (let ((classpath-jars (get-classpath-jars current-classloader)))
      (java:jcall "useOptions" memory-compiler
                  (java:jarray-from-list
                   (append
                    (if classpath-jars
                        (list "-classpath"
                              (format nil "~{~a~^: ~}" classpath-jars)))
                    (list "-Xlint:none")))))
    (java:jcall
     "compile"
     memory-compiler
     class-name
     (java:jcall "toString" string-builder))))
