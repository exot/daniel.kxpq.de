
(in-package :website)

;;;

(defwidget standard-poem-widget ()
  ((poem :accessor poem
         :initarg  :poem
         :type poem)))

(defmethod render-widget ((poem-widget standard-poem-widget) &key &allow-other-keys)
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

(defwidget foldable-poem-widget ()
  ((poem-widget :accessor poem-widget
                :initarg  :poem-widget
                :type     standard-poem-widget)
   (mode :accessor mode
         :initarg  :mode
         :type     (or :show :folded))))

(defmethod render-widget ((widget foldable-poem-widget) &key &allow-other-keys)
  (ecase (mode widget)
    (:show
     (print (mode widget))
     (with-html
       (:div :class "widget unfolded-poem"
         (:div :style "text-align:right"
           (render-link (f_% (setf (mode widget) :folded))
                        "Dismiss"))
         (render-widget (poem-widget widget)))))
    (:folded
     (print (mode widget))
     (let ((poem (poem (poem-widget widget))))
       (with-html
         (:div :class "widget folded-poem"
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
    (setf (widget-propagate-dirty widget)
          (list main-widget (widget-propagate-dirty widget)))
    widget))

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
                 (widget     (make-instance 'widget)))
            (setf (widget-children widget)
                  (mapcar #'make-widget
                          (list
                           (f_% (with-html
                                  (:div :class "poem-selector"
                                    (:h2 "Selected Poems")
                                    (:p "Here you may find a very personal collection of poems I like.")
                                    (:p "When I find some more time, you may also select poems by
                                        several criteria like author, title, contains...  But this
                                        needs to be implemented.  Right now, you just see all of
                                        them."))
                                  (:hr)))
                           (mapcan (lambda (poem)
                                     (list
                                      (make-foldable-poem-widget widget
                                                                 poem
                                                                 (poem-selector-poem-widget-class obj)
                                                                 :folded)
                                      (f_% (with-html (:hr)))))
                                   poems-list))))
            (values widget tokens nil :no-cache)))))

;;;

nil
