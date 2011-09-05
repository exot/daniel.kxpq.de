
(in-package :website)

;;;

(defview poem-table-view (:type table :inherit-from '(:scaffold poem))
  (body :present-as (short-text :max-length 40))
  (source :present-as (short-text :max-length 30)))
(defview poem-data-view  (:type data  :inherit-from '(:scaffold poem))
  (body :present-as paragraph))
(defview poem-form-view  (:type form  :inherit-from '(:scaffold poem))
  (body :present-as textarea))

;;;

nil