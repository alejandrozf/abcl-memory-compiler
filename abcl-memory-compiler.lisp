;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun get-classpath-jars (current-classloader)
  (cdar (java:dump-classpath current-classloader)))


(defmacro with-dynamic-compiler
    (body
     &key classloader extra-classpaths)
  `(let ((memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler")))

     (loop :for classpath :in ,extra-classpaths
           :do (java:jcall "addURL" ,classloader classpath))

     (java:jcall "useParentClassLoader" memory-compiler ,classloader)

     (let ((classpath-jars (get-classpath-jars ,classloader)))
       (java:jcall "useOptions" memory-compiler
                   (java:jarray-from-list
                    (append
                     (if classpath-jars
                         (list "-classpath"
                               (format nil "~{~a~^: ~}" classpath-jars)))
                     (list "-Xlint:none")))))
     (progn ,@body)))


(defun compile-to-multiple-classes (classes-code-pairs
                                    &key
                                      (classloader (java:jnew "org.armedbear.lisp.JavaClassLoader"))
                                      extra-classpaths)
  (with-dynamic-compiler
    ((loop :for (class-name class-source-code) :in classes-code-pairs
           :do (java:jcall "addSource" memory-compiler class-name class-source-code))

     (java:jcall
      "compileAll"
      memory-compiler))
    :classloader classloader
    :extra-classpaths extra-classpaths))


(defun compile-to-class (class-name source)
  (labels ((compile-to-class-aux (class-name-aux class-source-code-aux
                                  &key
                                    (classloader (java:jnew "org.armedbear.lisp.JavaClassLoader"))
                                    extra-classpaths)
           (with-dynamic-compiler
               ((java:jcall
                 "compile"
                 memory-compiler
                 class-name-aux
                 class-source-code-aux))
             :classloader classloader
             :extra-classpaths extra-classpaths)))
    (let ((classpath (uiop:split-string
                      (java:jstatic
                       "getProperty"
                       "java.lang.System"
                       "java.class.path"))))
      (compile-to-class-aux
       class-name
       source
       :classloader (java:get-current-classloader)
       :extra-classpaths
       (loop :for cp :in classpath
             :collect (unless (equal cp "")
                        (java:jcall "toURL" (java:jcall "toURI" (java:jnew "java.io.File" cp)))))))))


(defun compile-to-multiple-classes (classes-code-pairs)

  (labels ((compile-to-multiple-classes-aux (classes-code-pairs
                                             &key
                                               (classloader (java:jnew "org.armedbear.lisp.JavaClassLoader"))
                                               extra-classpaths)
           (with-dynamic-compiler
               ((loop :for (class-name class-source-code) :in classes-code-pairs
                      :do (java:jcall "addSource" memory-compiler class-name class-source-code))

                (java:jcall
                 "compileAll"
                 memory-compiler))
             :classloader classloader
             :extra-classpaths extra-classpaths)))
    (let ((classpath (uiop:split-string
                      (java:jstatic
                       "getProperty"
                       "java.lang.System"
                       "java.class.path"))))
      (compile-to-multiple-classes-aux
       classes-code-pairs
       :classloader (java:get-current-classloader)
       :extra-classpaths
       (loop :for cp :in classpath
             :collect (unless (equal cp "")
                        (java:jcall "toURL" (java:jcall "toURI" (java:jnew "java.io.File" cp)))))))))
