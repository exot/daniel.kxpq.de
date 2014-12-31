(in-package :website)

;;; Customize rendering

(defmethod render-page-headers :after ((app website))
  (with-html
    (:link :rel "shortcut icon" :href "")))

(defmethod render-page-body :before ((app website) rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "pageheader"
          (:a :href "/home" "Home")
          " ⋅ "
          (:a :href "/poetry" "Poetry"))))

(defmethod render-page-body :after ((app website) rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
      (:div :class "impressum"
        (:a :href "/impressum" "Impressum")))))

;;; Starting point

(defvar *initial-poem* nil
  "Poem displayed on the first page.")

(defun init-user-session (root)
  (unless *initial-poem*
    (setf *initial-poem* (find-poem-by-start "I went to the woods")))
  (let ((main-page (make-start-page)))
    (setf (widget-children root)
          (make-instance 'on-demand-selector
                         :lookup-function (lambda (selector tokens)
                                            (declare (ignore selector))
                                            (if (string= (first tokens) "impressum")
                                                (values (md "impressum.md") tokens nil nil)
                                                (values main-page nil tokens nil)))))))

;;; Start page

(defun make-start-page ()
  (make-instance 'static-selector
                 :panes (list (cons "home"   (make-home-page))
                              (cons "math"   (make-math-page))
                              (cons "poetry" (make-poems-page))
                              (cons "fun"    (make-fun-page))
                              (cons "impressum" (md "impressum.md")))))

;;; Fun

(defun make-fun-page ()
  (make-widget (f_%
                 (with-html
                   (:h1 "Bla"))
                 (render-link (f_% (print "Woo!")) "Erm?"))))

;;; Home

(defun make-home-page ()
  (make-widget
   (f_% (with-html
          (when *initial-poem*
            (render-widget (make-instance 'standard-poem-widget
                                          :poem *initial-poem*)))))))

;;; Math

(defun make-math-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main" (md "math.md")))))

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
                       (:p (:a :href (make-webapp-uri "/poetry/list")
                               "List Poems"))))
                (make-instance 'gridedit
                               :name 'poems-grid
                               :drilldown-type :view
                               :data-class 'poem
                               :view 'poem-table-view
                               :item-data-view 'poem-data-view
                               :item-form-view 'poem-form-view))))

;;; Pensieve

(defun make-pensieve-page ()
  (make-instance 'on-demand-selector
                 :lookup-function (lambda (selector tokens)
                                    (declare (ignore selector))
                                    (values (if (or (null tokens)
                                                    (string= (first tokens) "")
                                                    (string= (first tokens) "main"))
                                                (md "pensieve/main.md")
                                                (md (concatenate 'string
                                                                 "pensieve/" (first tokens) ".md"))) ;safe?
                                            tokens
                                            nil
                                            nil)))) ;don't cache

;;;

nil
