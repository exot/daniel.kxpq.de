
(in-package :website)

(defview poem-table-view (:type table :inherit-from '(:scaffold user)))
(defview poem-data-view  (:type data  :inherit-from '(:scaffold user)))
(defview poem-form-view  (:type form  :inherit-from '(:scaffold user)))