
(in-package :website)

;; Define callback function to initialize new sessions

(defun init-user-session (root)
  (setf (widget-children root)
        (make-navigation "Main Menu"
                         (list "first" (make-users-gridedit))
                         (list "other" (make-users-gridedit))
                         (list "google" (f_% (redirect "http://www.google.de"))))))

(defclass user ()
  ((id)
   (name :accessor user-name
         :initarg :name
         :initform ""
         :type string)))

(defun make-users-gridedit ()
  (make-instance 'gridedit
                 :name 'users-grid
                 :data-class 'user
                 :view 'user-table-view
                 :widget-prefix-fn (lambda (&rest args)
                                     (declare (ignore args))
                                     (with-html (:h1 "Users")))
                 :item-data-view 'user-data-view
                 :item-form-view 'user-form-view))

(defview user-table-view (:type table :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))