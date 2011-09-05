
(in-package :website)

;;;

(defwidget standard-poem-widget ()
  ((poem :accessor poem
         :initarg  :poem
         :type poem)))

(defmethod render-widget ((poem-widget standard-poem-widget) &key inlinep)
  (declare (ignore inlinep))
  (let ((poem (poem poem-widget)))
    (with-html
      (:div :class "view standard-poem"
            :style "padding-top:3ex;padding-bottom:3ex;"
        (when (poem-title poem)
          (htm (:div :class "poem-title"
                 (str (poem-title poem)))))
        (:div :class "poem-body"
          (str (poem-body poem)))
        (:div :class "poem-author"
          (str (poem-author poem)))))))

(defwidget poem-selector ()
  ((filter :accessor poem-filter
           :initarg  :filter
           :type     function)
   (poem-widget-class
           :accessor poem-selector-poem-widget-class
           :initarg  :poem-widget-class
           :type     (or class symbol)))
  (:documentation "Displays poems, selected by filter function."))

(defmethod render-widget ((obj poem-selector) &key inlinep &allow-other-keys)
  (declare (ignore inlinep))
  (with-html
    (:hr)
    (:div :class "poem-selector"
       (loop
          :for poem :in (find-persistent-objects *store* 'poem)
          :when (funcall (poem-filter obj) poem)
          :do (render-widget (make-instance (poem-selector-poem-widget-class obj)
                                            :poem poem))
              (with-html (:hr))))))

;;;

nil
