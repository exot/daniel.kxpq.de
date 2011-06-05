
(in-package :website)

;; Define callback function to initialize new sessions

(defun init-user-session (root)
  (setf (widget-children root)
        (make-navigation "Main Menu"
                         (list "first"
                               (make-instance 'widget
                                              :children (list
                                                         (make-instance 'flash
                                                                        :messages (list "Welcome!"))
                                                         (make-users-gridedit))))
                         (list "other" (make-users-gridedit))
                         (list "google" (f_% (redirect "http://www.google.de"))))))

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
