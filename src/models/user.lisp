
(in-package :website)

(defclass user ()
  ((id)
   (name :accessor user-name
         :initarg :name
         :initform ""
         :type string)
   (info :accessor user-info
         :initarg :info
         :type string)
   (password-hash :accessor user-password-hash
                  :initarg :password-hash
                  :type string)))
