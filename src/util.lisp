
(in-package :website)

;;;

(setf (gethash "lisp" hunchentoot::*mime-type-hash*) "text/plain; charset=utf-8")

;;; Useful functions

(defmethod make-widget ((obj list))
  (make-instance 'widget :children (mapcar #'make-widget obj)))

(defun to-come ()
  "Writes HTML code to say that there is \"More to come\"."
  (with-html
    (:p :style "font-style:italic"
        "More to come")))

(defun md (path)
  "Writes HTML output generated from the markdown file found in path."
  (make-widget (let ((html nil))
                 (f_%
                   (if (not html)
                       (multiple-value-bind (out err ret)
                           (trivial-shell:shell-command
                            (concatenate 'string
                                         "cat src/pages/"
                                         path
                                         " | markdown-extra"))
                         (if (not (zerop ret))
                             (princ err *weblocks-output-stream*)
                             (progn
                               (setf html out)
                               (princ html *weblocks-output-stream*))))
                       (princ html *weblocks-output-stream*))))))


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