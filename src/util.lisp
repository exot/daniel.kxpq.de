
(in-package :website)

;;; A Fix for Metatilities

(setf (symbol-function 'metatilities:fixnump)
      (symbol-function 'ccl:fixnump))

;;; Useful functions

(defmethod make-widget ((obj list))
  (make-instance 'widget :children (mapcar #'make-widget obj)))

(defun to-come ()
  "Writes HTML code to say that there is \"More to come\"."
  (with-html
    (:p :style "font-style:italic"
        "More to come")))

(defun html-from-markdown (path)
  "Writes HTML output generated from the markdown file found in path."
  (let* ((default-path (make-pathname :directory (append (pathname-directory (asdf-system-directory :website))
                                                         (list "src" "pages"))))
         (html         (with-output-to-string (s)
                         (cl-markdown:markdown (merge-pathnames default-path path)
                                               :stream s))))
    (make-widget
     (f_% (princ html *weblocks-output-stream*)))))

;;;

nil