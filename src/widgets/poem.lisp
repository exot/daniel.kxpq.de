
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
      (:div :class "widget standard-poem"
            :style "padding-top:3ex;padding-bottom:3ex;"
        (when (poem-title poem)
          (htm (:div :class "poem-title"
                 (str (poem-title poem)))))
        (:div :class "poem-body"
          (str (poem-body poem)))
        (:div :class "poem-author"
          (str (poem-author poem)))))))

;;;

(defwidget poem-selector (on-demand-selector)
  ((poem-widget-class :accessor poem-selector-poem-widget-class
                      :initarg  :poem-widget-class
                      :type     (or class symbol))
   (poem-list :type list)))

(defmethod initialize-instance :after ((obj poem-selector) &key &allow-other-keys)
  (setf (on-demand-lookup-function obj)
        (lambda (obj tokens)
          (let* ((poems-list (find-persistent-objects *store* 'poem))
                 (widget     (make-widget
                              (f_% (with-html
                                     (:hr)
                                     (:div :class "poem-selector"
                                       (loop
                                          :for poem :in poems-list
                                          :do (render-widget (make-instance (poem-selector-poem-widget-class obj)
                                                                            :poem poem))
                                              (with-html (:hr)))))))))
            (values widget tokens nil :no-cache)))))

;;;

nil
