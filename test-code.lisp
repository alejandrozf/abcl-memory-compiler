(defun load-packages ()
  (let* ((source (alexandria:read-file-into-string "/home/alejandrozf/projects/abcl-memory-compiler/JoinClassLoader.java"))
         (joinclassloader-class (abcl-memory-compiler:compile-to-class
                                 "org.azf.JoinClassLoader"
                                 source))
         (joinclassloader-instance
           (jnew joinclassloader-class
                 (jnew "org.armedbear.lisp.JavaClassLoader")
                 (jarray-from-list (list (java:jnew "org.armedbear.lisp.JavaClassLoader"
                                                  (java:jcall "getClassLoader"
                                                              (java:jclass "org.armedbear.lisp.LispObject")))
                                         (get-current-classloader))))))
    (jcall "getGetPackages" joinclassloader-instance)))
