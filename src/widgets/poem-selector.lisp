
(in-package :website)

;;;

(defwidget poem-selector ()
  ((filter :accessor poem-filter
           :initarg  :filter
           :type     function)
   (view   :accessor poem-view
           :initarg  :view
           :type     symbol))
  (:documentation "Displays poems, selected by filter function."))

(defmethod render-widget ((obj poem-selector) &key inlinep &allow-other-keys)
  (declare (ignore inlinep))
  (let ((view (make-instance (poem-view obj))))
    (with-html
      (:hr)
      (:div :class "poem-selector"
         (loop
            :for poem :in (find-persistent-objects *store* 'poem)
            :when (funcall (poem-filter obj) poem)
            :do (render-object-view poem view) (with-html (:hr)))))))

;;;

nil
