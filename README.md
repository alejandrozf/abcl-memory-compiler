# abcl-memory-compiler
### _Alejandro Zamora Fonseca <ale2014.zamora@gmail.com>_

A way to compile source code for create Java classes at runtime!
This a wrapper over the library InMemoryJavaCompiler(https://github.com/trung/InMemoryJavaCompiler)

Usage:

```
CL-USER> (ql:quickload :abcl-memory-compiler)
To load "abcl-memory-compiler":
  Load 1 ASDF system:
    abcl-memory-compiler
; Loading "abcl-memory-compiler"
[package abcl-memory-compiler]
(:ABCL-MEMORY-COMPILER)
CL-USER> (abcl-memory-compiler:compile-to-class "org.azf.HelloClass"
          "package org.azf;
           public class HelloClass {
                  public String hello() { return \"hello\"; }
           }")
#<java class org.azf.HelloClass>
CL-USER> (jnew *)
#<org.azf.HelloClass org.azf.HelloClass@32eaa91 {57415E14}>
CL-USER> (jcall "hello" *)
"hello"
CL-USER> (abcl-memory-compiler:compile-to-class
          "org.azf.lisp.Funcaller"
          "package org.azf.lisp;
          import org.armedbear.lisp.LispObject;
          public class Funcaller {
                  public LispObject funcall(LispObject obj) {return obj.execute();}
          }")
#<java class org.azf.lisp.Funcaller>
CL-USER> (jnew *)
#<org.azf.lisp.Funcaller org.azf.lisp.Funcaller@23e856d1 {13D049D6}>
CL-USER> (jcall "funcall" * (lambda () 1))
1
CL-USER> (abcl-memory-compiler:compile-to-multiple-classes
          (list (list "A" "public class A{ public B b() { return new B(); }}")
                (list "B" "public class B{ public String toString() { return \"B!\"; }}")))
#<java.util.HashMap {A=class A, B=class B} {367B9E1D}>
CL-USER>
```


## License

MIT
