
(in-package :website)

;;;

(defview poem-table-view (:type table :inherit-from '(:scaffold poem))
  (body :present-as (short-text :max-length 40)))
(defview poem-data-view  (:type data  :inherit-from '(:scaffold poem))
  (body :present-as paragraph))
(defview poem-form-view  (:type form  :inherit-from '(:scaffold poem))
  (body :present-as textarea))

;;; View poems

(defclass selected-poems-view (view)
  ())

(defclass selected-poems-view-field (view-field)
  ())

(defclass selected-poems-scaffold (scaffold)
  ())

;;;

nil