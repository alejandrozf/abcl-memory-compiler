;;;; abcl-memory-compiler.asd

(asdf:defsystem #:abcl-memory-compiler
  :description "Compile Java source code to runtime classes"
  :author "Alejandro Zamora Fonseca <ale2014.zamora@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:mvn "org.mdkt.compiler/InMemoryJavaCompiler/1.3.0")
               (:file "package")
               (:file "abcl-memory-compiler")))
