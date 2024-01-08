;;;; abcl-memory-compiler.lisp

(in-package #:abcl-memory-compiler)


(defun compile-to-class (class-name class-source-code)
  (let ((string-builder (java:jnew "java.lang.StringBuilder"))
        (memory-compiler (java:jstatic "newInstance" "org.mdkt.compiler.InMemoryJavaCompiler"))
        (current-classloader (java:get-current-classloader)))

    (java:jcall "append" string-builder class-source-code)

    (java:jcall "useParentClassLoader" memory-compiler current-classloader)

    (java:jcall "useOptions" memory-compiler
                  (java:jarray-from-list
                   (list "-classpath"
                         "/Users/alejandrozf/.m2/repository/org/java-websocket/Java-WebSocket/1.5.4/Java-WebSocket-1.5.4.jar"
                         "-Xlint:none")))
    ;; TODO: see (java:dump-classpath (java:get-current-classloader))

    ;; (cl-user::break-args memory-compiler)


    #+nil(let ((sb (java:jnew "java.lang.StringBuilder"))
          (resources (java:jcall "getResources" current-classloader "/"))
          (path-separator (java:jfield "java.io.File" "pathSeparator")))
      (loop :while (java:jcall "hasMoreElements" resources)
            :do (java:jcall "append" sb
                            (java:jcall "getFile" (java:jcall "nextElement" resources)))
                (java:jcall "append" sb
                            path-separator))

      ;; (cl-user::break-args sb)


      (java:jcall "useOptions" memory-compiler
                  (java:jarray-from-list
                   (list "-classpath" "/Users/alejandrozf/.m2/*"))))

    ;; (cl-user::break-args memory-compiler)

    (java:jcall
     "compile"
     memory-compiler
     class-name
     (java:jcall "toString" string-builder))))
