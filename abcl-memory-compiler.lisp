;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun get-classpath-jars (current-classloader)
  (cdar (java:dump-classpath current-classloader)))


(defun compile-to-class (class-name class-source-code
                         &key
                           (classloader
                            (java:jnew "org.armedbear.lisp.JavaClassLoader"
                                       (java:jcall "getClassLoader"
                                                   (java:jclass "org.armedbear.lisp.LispObject"))))
                           extra-classpaths)
  (let ((string-builder (java:jnew "java.lang.StringBuilder"))
        (memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler"))
        (current-classloader classloader))

    (java:jcall "append" string-builder class-source-code)

    (loop :for classpath :in extra-classpaths
          :do (java:jcall "addURL" current-classloader classpath))

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


(defun compile-to-multiple-classes
    (classes-code-pairs
     &key
       (classloader
        (java:jnew "org.armedbear.lisp.JavaClassLoader"
                   (java:jcall "getClassLoader"
                               (java:jclass "org.armedbear.lisp.LispObject"))))
       extra-classpaths)
  (let ((memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler"))
        (current-classloader classloader))

    (loop :for classpath :in extra-classpaths
          :do (java:jcall "addURL" current-classloader classpath))

    (java:jcall "useParentClassLoader" memory-compiler current-classloader)

    (let ((classpath-jars (get-classpath-jars current-classloader)))
      (java:jcall "useOptions" memory-compiler
                  (java:jarray-from-list
                   (append
                    (if classpath-jars
                        (list "-classpath"
                              (format nil "~{~a~^: ~}" classpath-jars)))
                    (list "-Xlint:none")))))

    (loop :for (class-name class-source-code) :in classes-code-pairs
          :do (java:jcall "addSource" memory-compiler class-name class-source-code))

    (java:jcall
     "compileAll"
     memory-compiler)))
