
(in-package :website)

;;;

(defvar *initial-poem* nil
  "Poem displayed on the first page.")

(defun init-user-session (root)
  (unless *initial-poem*
    (setf *initial-poem* (find-poem-by-start "I went to the woods")))
  (setf (widget-children root)
        (make-start-page)))

;;;

nil