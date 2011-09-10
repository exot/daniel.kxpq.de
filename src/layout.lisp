
(in-package :website)

;;; Customize rendering

(defmethod render-navigation-menu ((obj navigation) &rest args &key menu-args &allow-other-keys)
  (declare (ignore args))
  (apply #'render-menu (navigation-menu-items obj)
         :base (selector-base-uri obj)
         :selected-pane nil
         :header (navigation-header obj)
         :container-id (dom-id obj)
         :empty-message "No navigation entries"
         menu-args))

(defmethod render-page-body :after ((app website) rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
          :style "color:white"
      (:a :href "http://common-lisp.net"
          (:img :src (make-webapp-public-file-uri "images/footer/lisp-lizard.png")
                :alt "Public Projects for Common Lisp"))
      (str "&nbsp;")
      (:a :href "http://www.lisp.org"
          (:img :src (make-webapp-public-file-uri "images/footer/lambda-lisp.png")
                :alt "This page is made with Common Lisp."))
      (str "&nbsp;")
      (:a :href "http://common-lisp.net/project/cl-markdown/"
          (:img :src (make-webapp-public-file-uri "images/footer/cl-markdown.png")
                :alt "Occasionally using CL-Markdown."))
      (str "&nbsp;")
      (:a :href "http://www.catb.org/hacker-emblem/"
          (:img :src (make-webapp-public-file-uri "images/footer/hacker.png")
                :alt "Thou hacking shall be beautiful.")))))

;;; Starting point

(defun make-start-page ()
  (make-widget (list
                (make-instance 'header
                               :default-title "Daniel's Playground"
                               :title-list '(("home" "At Home")
                                             ("math" "On Math")
                                             ("poems" "Poems")
                                             ("lisp" "Lisp")
                                             ("conexp-clj" "Conexp in Clojure")))
                (make-navigation "Main Menu"
                                 "home"       (make-home-page)
                                 "math"       (make-math-page)
                                 "poems"      (make-poems-page)
                                 "lisp"       (make-common-lisp-page)
                                 "conexp-clj" (make-conexp-clj-page)))))

;;; Home

(defun make-home-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (make-widget
                                (f_% (with-html
                                       (when *initial-poem*
                                         (render-widget (make-instance 'standard-poem-widget
                                                                       :poem *initial-poem*)))
                                       (render-widget (html-from-markdown "main.md"))))))
                         (cons "personal"
                               (html-from-markdown "personal.md")))))

;;; Math

(defun make-math-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (html-from-markdown "math.md"))
                         (cons "fca"
                               (html-from-markdown "fca.md")))))

;;; conexp-clj

(defun make-conexp-clj-page ()
  (html-from-markdown "conexp-clj.md"))

;;; Poems

(defun make-poems-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "list" (make-poem-list-page))
                         (cons "edit" (make-instance 'login-maybe
                                                     :child-widget (f_% (make-poem-edit-page))
                                                     :on-login #'check-login)))))

(defun make-poem-list-page ()
  (make-instance 'poem-selector
                 :poem-widget-class 'standard-poem-widget))

(defun make-poem-edit-page ()
  (make-widget (list
                (f_% (with-html
                       (:p (:a :href (make-webapp-uri "/poems/list")
                               "List Poems"))))
                (make-instance 'gridedit
                               :name 'poems-grid
                               :drilldown-type :view
                               :data-class 'poem
                               :view 'poem-table-view
                               :item-data-view 'poem-data-view
                               :item-form-view 'poem-form-view))))

;;; Common Lisp

(defun make-common-lisp-page ()
  (html-from-markdown "lisp.md"))

;;;

nil