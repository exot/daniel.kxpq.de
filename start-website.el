(require 'slime)
(slime-start :program "ccl"
             :directory (expand-file-name "~/lisp/cl/source/website/")
             :program-args '("-e" "(progn
                                     (ql:quickload :website)
                                     (funcall (read-from-string \"website:start-website\")))"))
