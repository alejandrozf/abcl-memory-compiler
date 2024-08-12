(defun compile-java-code-to-abcl (class-name)
  (interactive "sEnter the class name: ")
  (slime-repl-eval-string
   (format "(abcl-memory-compiler:compile-to-class
              \"%s\"
              \"%s\")"
           class-name
           (let ((start (point-min))
                 (end (point-max)))
             (buffer-substring-no-properties start end)))))


(global-set-key (kbd "<f5> j") 'compile-java-code-to-abcl)
