(in-package :website)

;;;

(defwidget standard-poem-widget ()
  ((poem :accessor poem
         :initarg  :poem
         :type poem)))

(defmethod render-widget-body ((poem-widget standard-poem-widget) &key &allow-other-keys)
  (let ((poem (poem poem-widget)))
    (with-html
      (:div
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
  (ecase (mode widget)
    (:show
     (with-html
       (:div :class "dismiss-button"
         (render-link (f_% (setf (mode widget) :folded)
                           (mark-dirty widget))
                      "Hide"))
       (render-widget (poem-widget widget))))
    (:folded
     (let ((poem (poem (poem-widget widget))))
       (with-html
         (:div :class "folded-poem"
           (:div :class "poem-description"
             (str (or (and (poem-title poem)
                           (concatenate 'string "«" (poem-title poem) "»"))
                      (concatenate 'string
                                   "["
                                   (subseq (poem-body poem)
                                           0
                                           (or (position #\Newline (poem-body poem))
                                               (length (poem-body poem))))
                                   "]")))
             " by "
             (str (poem-author poem)))
           (:div :class "show-button"
             (render-link (f_% (setf (mode widget) :show)
                               (mark-dirty widget))
                          "Show"))))))))

(defun make-foldable-poem-widget (poem poem-widget-class mode)
  (let ((widget (make-instance 'foldable-poem-widget
                               :poem-widget (make-instance poem-widget-class
                                                           :poem poem)
                               :mode mode)))
    widget))

;;;

(defwidget poem-selector (on-demand-selector)
  ((poem-widget-class :accessor poem-selector-poem-widget-class
                      :initarg  :poem-widget-class
                      :type     (or class symbol))
   (selection         :type     list)
   (last-access       :type     integer)))

(defun select-poems-by-tokens (selector tokens)
  (when (or (not (slot-boundp selector 'selection))
            (not (equalp tokens (slot-value selector 'selection)))
            (> *last-store-modification-time*
               (slot-value selector 'last-access)))
    (setf (slot-value selector 'last-access)
          (get-universal-time))
    (setf (widget-children selector :poems)
          (mapcar (lambda (poem)
                    (make-foldable-poem-widget poem
                                               (poem-selector-poem-widget-class selector)
                                               :folded))
                  (sort (find-persistent-objects *store* 'poem)
                        #'string<=
                        :key (lambda (poem) (poem-title poem)))))
    (setf (slot-value selector 'selection)
          tokens)))

(defmethod render-widget-body ((obj poem-selector) &rest args)
  (declare (ignore args))
  (with-html
    (:p :style "margin-bottom: 2em;"
        "Here you may find a personal collection of poems I like.")))

(defmethod render-widget-children ((obj poem-selector) &rest args)
  (with-html (:hr))
  (mapc (lambda (child)
          (apply #'render-widget child args)
          (with-html (:hr)))
        (widget-children obj :poems)))

(defmethod initialize-instance :after ((obj poem-selector) &key &allow-other-keys)
  (setf (on-demand-lookup-function obj)
        (lambda (obj tokens)
          (select-poems-by-tokens obj tokens)
          (values (make-widget "") tokens nil :no-cache))))

;;;

nil
