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
          (:a :href "/essays" "Essays")
          " ⋅ "
          (:a :href "/poetry" "Poetry")
          " ⋅ "
          (:a :href "/about" "About"))))

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
  (let ((page-list (list (cons "home"      (make-home-page))
                         (cons "math"      (make-math-page))
                         (cons "poetry"    (make-poems-page))
                         (cons "fun"       (make-fun-page))
                         (cons "impressum" (md "impressum.md"))
                         (cons "essays"    (make-essay-page))
                         (cons "about"     (md "about.md"))
                         (cons "internal"  (make-admin-page)))))
    (make-instance 'on-demand-selector
                   :lookup-function (lambda (selector tokens)
                                      (declare (ignore selector))
                                      (let ((widget (or (and (eq nil (first tokens))
                                                             (cdr (first page-list)))
                                                        (cdr (assoc (first tokens) page-list
                                                                    :test #'equalp))
                                                        (make-widget
                                                         (f_%
                                                           (with-html
                                                             (:div :class "http-not-found"
                                                                   "Sorry, that page does not exist.")))))))
                                        (values widget (list (first tokens)) (rest tokens) :no-cache))))))
;;; Fun (i.e., trying things out)

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

;;; Essays

(defun make-essay-page ()
  (make-widget
   (f_% (with-html
          (:div :class "essays"
                "Things I wrote about some time ago.  Enjoy!"
                (:div :class "table-of-contents"
                      (:ul (:li (:a :href "#on-the-pragmatic-power-of-programming-languages"
                                    "On the Pragmatic Power of Programming Languages"))
                           (:li (:a :href "#maintaining-understandability"
                                    "Maintaining Understandability"))))
                (:div :class "texts"
                      (:hr)
                      (render-widget (md "pensieve/pragmatic-power.md"))
                      (:hr)
                      (render-widget (md "pensieve/maintaining-understandability.md"))))))))

;;; Internal

(defparameter *now* (local-time:now))

(defun make-admin-page ()
  (make-instance 'login-maybe
                 :child-widget (f_% (make-widget
                                     (f_% (with-html
                                            (:table :class "internal-data"
                                             (:tr (:td "Started")
                                                  (:td (str *now*)))
                                             (:tr (:td "Lisp")
                                                  (:td (str (lisp-implementation-type))
                                                       " "
                                                       (str (lisp-implementation-version))))
                                             (:tr (:td "Hunchentoot")
                                                  (:td (str hunchentoot:*hunchentoot-version*)))
                                             (:tr (:td "Sessions")
                                                  (:td (str (length (active-sessions))))))
                                            (:br)
                                            (:div :class "logs"
                                                  (:h3 "message.log")
                                                  (:div
                                                   (str (alexandria:read-file-into-string
                                                         #P"message.log")))
                                                  (:h3 "access.log")
                                                  (:div
                                                   (str (alexandria:read-file-into-string
                                                         #P"access.log"))))
                                            (:br)
                                            (render-link (f_% (reset-sessions))
                                                         "Reset Sessions")
                                            (:br)
                                            (render-link (f_% (ql:quickload :website))
                                                         "Reload Website")
                                            " (may not work)"))))
                 :on-login #'check-login))

;;;

nil
