
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
                          "home"       (make-home-page)
                          "math"       (make-math-page)
                          "poems"      (make-poems-page)
                          "lisp"       (make-common-lisp-page)
                          "conexp-clj" (make-conexp-clj-page)))))

(defmethod render-page-body :before ((app website) rendered-html)
  (declare (ignore rendered-html))
  nil)

(defmethod render-page-body :after ((app website) rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
          :style "color:white"
      (:p "Running on "
        (str (concatenate 'string (server-type) " " (server-version)))
        " ("
        (str (concatenate 'string
                          (lisp-implementation-type) " "
                          (lisp-implementation-version)))
        ")")
      (:a :href "http://www.lisp.org"
          (:img :src (make-webapp-public-file-uri "images/footer/lambda-lisp.png")
                :alt "This page is made with Common Lisp."))
      (str "&nbsp;")
      (:a :href "http://www.catb.org/hacker-emblem/"
          (:img :src (make-webapp-public-file-uri "images/footer/hacker.png")
                :alt "Thou hacking shall be beautiful.")))))

;;;

nil