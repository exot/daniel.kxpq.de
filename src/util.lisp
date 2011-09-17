
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
         (html nil))
    (make-widget
     (f_%
       (unless html
         (setf html (with-output-to-string (s)
                      (cl-markdown:markdown (merge-pathnames default-path path)
                                            :stream s))))
       (princ html *weblocks-output-stream*)))))

;;; Record time when database is changed

(defvar *last-store-modification-time* (get-universal-time))

(defmethod persist-object :after ((store (eql *store*)) obj &key &allow-other-keys)
  (declare (ignore obj))
  (setf *last-store-modification-time* (get-universal-time)))

(defmethod delete-persistent-object :after ((store (eql *store*)) obj)
  (declare (ignore obj))
  (setf *last-store-modification-time* (get-universal-time)))

(defmethod delete-persistent-object-by-id :after ((store (eql *store*)) class-name id)
  (declare (ignore class-name id))
  (setf *last-store-modification-time* (get-universal-time)))

;;;

nil