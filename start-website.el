(require 'slime)
(slime-start :program "ccl"
             :directory (expand-file-name "~/Documents/lang/lisp/cl/source/website/")
             :program-args '("-e" "(progn
                                     (ql:quickload :weblocks-memory)
                                     (ql:quickload :website)
                                     (funcall (read-from-string \"website:start-website\")))"))
