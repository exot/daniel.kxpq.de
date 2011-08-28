
(in-package :website)

;;; Define callback function to initialize new sessions

(defun init-user-session (root)
  (setf (widget-children root)
        (list
         (make-widget
          (f_% (with-html
                 (:h1 :style "text-align:left;padding:0 0 2em 3em;"
                      "Daniel's Playground"))))
         (make-navigation "Main Menu"
                          (list "home"       #'make-home-page)
                          (list "math"       #'make-math-page)
                          (list "poems"      #'make-poems-page)
                          (list "lisp"       #'make-common-lisp-page)
                          (list "conexp-clj" #'make-conexp-clj-page)))))

(defmethod render-page-body :after ((app website) rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
      (:p "Running on "
        (str (concatenate 'string (server-type) " " (server-version)))
        " ("
        (str (concatenate 'string
                          (lisp-implementation-type) " "
                          (lisp-implementation-version))) ")")
        (:img :src (make-webapp-public-file-uri "images/footer/xhtml.png")
              :alt "This site has valid XHTML 1.1.")
        (str "&nbsp;")
        (:a :href "http://www.lisp.org"
            (:img :src (make-webapp-public-file-uri "images/footer/lambda-lisp.png")
                  :alt "This page is made with Common Lisp."))
        (str "&nbsp;")
        (:a :href "http://www.catb.org/hacker-emblem/"
            (:img :src (make-webapp-public-file-uri "images/footer/hacker.png")
                  :alt "Thou Hacking shall be beautiful.")))))

;;;

(defun make-home-page ()
  (with-html
    (:h2 "At Home")
    (:p "There is nothing to be seen here yet.")))

(defun make-math-page ()
  (with-html
    (:h2 "About Math")))

(defun make-personal-page ()
  (with-html))

(defun make-conexp-clj-page ()
  (redirect "http://www.math.tu-dresden.de/~borch/conexp-clj/"))

(defun make-poems-page ()
  (with-html
    (:h2 "Poems")))

(defun make-common-lisp-page ()
  (with-html
    (:h2 "Common Lisp the Language")
    (:a :href (make-webapp-public-file-uri "init-session.lisp")
        "Link")
    (:pre "(+ 1 2 3)")))


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