
(in-package :website)

;;;

(defwidget standard-poem-widget ()
  ((poem :accessor poem
         :initarg  :poem
         :type poem)))

(defmethod render-widget-body ((poem-widget standard-poem-widget) &key &allow-other-keys)
  (let ((poem (poem poem-widget)))
    (with-html
      (:div :style "padding-top:3ex;padding-bottom:3ex;"
        (when (poem-title poem)
          (htm (:div :class "poem-title"
                 (str (poem-title poem)))))
        (:div :class "poem-body"
          (str (cl-ppcre:regex-replace-all #\Newline
                                           (poem-body poem)
                                           "<br/>")))
        (:div :class "poem-author"
          (str (poem-author poem)))))))

;;;

(defwidget foldable-poem-widget ()
  ((poem-widget :accessor poem-widget
                :initarg  :poem-widget
                :type     standard-poem-widget)
   (mode :accessor mode
         :initarg  :mode
         :type     (or :show :folded))))

(defmethod render-widget-body ((widget foldable-poem-widget) &key &allow-other-keys)
  (print (mode widget))
  (ecase (mode widget)
    (:show
     (with-html
       (:div :style "text-align:right"
         (render-link (f_% (setf (mode widget) :folded))
                      "Dismiss"))
       (render-widget (poem-widget widget))))
    (:folded
     (let ((poem (poem (poem-widget widget))))
       (with-html
         (:div :class "folded-poem"
           (render-link (f_% (setf (mode widget) :show))
                        (or (poem-title poem)
                            (concatenate
                             'string
                             "["
                             (subseq (poem-body poem)
                                     0
                                     (or (position #\Newline (poem-body poem))
                                         (length (poem-body poem))))
                             "]")))
           " by "
           (str (poem-author poem))))))))

(defun make-foldable-poem-widget (main-widget poem poem-widget-class mode)
  (let ((widget (make-instance 'foldable-poem-widget
                               :poem-widget (make-instance poem-widget-class
                                                           :poem poem)
                               :mode mode)))
    widget))

;;;

(defwidget poem-selector (on-demand-selector)
  ((poem-widget-class :accessor poem-selector-poem-widget-class
                      :initarg  :poem-widget-class
                      :type     (or class symbol))))

(defmethod render-widget-body ((obj poem-selector) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "poem-selector"
      (:h2 "Selected Poems")
      (:p "Here you may find a very personal collection of poems I like."))))

(defmethod render-widget-children ((obj poem-selector) &rest args)
  (with-html (:hr))
  (mapc (lambda (child)
            (apply #'render-widget child args)
            (with-html (:hr)))
        (widget-children obj :poems)))

(defmethod initialize-instance :after ((obj poem-selector) &key &allow-other-keys)
  (setf (widget-children obj :poems)
        (mapcar (lambda (poem)
                  (make-foldable-poem-widget obj
                                             poem
                                             (poem-selector-poem-widget-class obj)
                                             :folded))
                (find-persistent-objects *store* 'poem)))
  (setf (on-demand-lookup-function obj)
        (lambda (obj tokens)
          (declare (ignore obj))
          (values (make-widget "") tokens nil :no-cache))))

;;;

nil
