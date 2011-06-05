
(in-package :website)

(defclass user ()
  ((id)
   (name :accessor user-name
         :initarg :name
         :initform ""
         :type string)))
