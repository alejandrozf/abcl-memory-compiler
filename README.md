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
CL-USER> (abcl-memory-compiler:compile-to-class "org.mdkt.HelloClass"
          "package org.mdkt;
           public class HelloClass {
                  public String hello() { return \"hello\"; }
           }")
#<java class org.mdkt.HelloClass>
CL-USER> (jnew *)
#<org.mdkt.HelloClass org.mdkt.HelloClass@32eaa91 {57415E14}>
CL-USER> (jcall "hello" *)
"hello"
CL-USER>
```

## License

MIT
