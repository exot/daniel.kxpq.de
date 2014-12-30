(in-package :website)

;;;

(defwidget header (on-demand-selector)
  ((default-title :accessor header-default-title
                  :initarg :default-title
                  :type string
                  :initform "")
   (title-fn :accessor header-title-fn
             :type function
             :initarg :title-fn)))

(defmethod initialize-instance :after ((header header) &key &allow-other-keys)
  (setf (on-demand-lookup-function header)
        (lambda (header tokens)
          (let ((title nil))
            (if (null tokens)
                (setf title (funcall (header-title-fn header) (header-default-title header)))
                (setf title (or (funcall (header-title-fn header) (first tokens))
                                (header-default-title header))))
            (values (make-widget
                     (f_% (with-html
                            (:h1 (:div :class "mainheader" (str title))))))
                    nil
                    tokens
                    :no-cache)))))

;;;

nil
