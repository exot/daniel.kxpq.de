
(in-package :website)

;;; Define callback function to initialize new sessions

(defun init-user-session (root)
  (setf (widget-children root)
        (make-navigation "Main Menu"
                         (list "home"
                               (f_%
                                 (with-html
                                   (:h1 "At Home"))))
                         (list "math"
                               (f_% (with-html (:a :href "src/init-session.lisp" "Link"))))
                         (list "personal"
                               "")
                         (list "conexp-clj"
                               (f_% (redirect "http://www.math.tu-dresden.de/~borch/conexp-clj/"))))))


;;; Left from ealier experiments

(defun make-users-gridedit ()
  (make-instance 'gridedit
                 :name 'users-grid
                 :data-class 'user
                 :view 'user-table-view
                 :widget-prefix-fn (f_% (with-html (:h1 "Users")))
                 :item-data-view 'user-data-view
                 :item-form-view 'user-form-view))

;;;

nil