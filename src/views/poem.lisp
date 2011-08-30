
(in-package :website)

;;;

(defview poem-table-view (:type table :inherit-from '(:scaffold poem))
  (body :present-as (short-text :max-length 40)))
(defview poem-data-view  (:type data  :inherit-from '(:scaffold poem))
  (body :present-as paragraph))
(defview poem-form-view  (:type form  :inherit-from '(:scaffold poem))
  (body :present-as textarea))


;;; View poems

(defclass poem-print-view (view)
  ())

(defmethod render-object-view-impl ((poem poem) (view poem-print-view) widget &rest args)
  (declare (ignore args widget))
  (with-html
    (:div :class "view poem-print"
          :style "padding-top:3ex;padding-bottom:3ex;"
      (:div :class "poem-title"
         (str (poem-title poem)))
      (:div :class "poem-body"
         (str (poem-body poem)))
      (:div :class "poem-author"
         (str (poem-author poem))))))

;;;

nil