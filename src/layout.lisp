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
          :style "color:white"
      ;; (:div :style "position:absolute;right:1em"
      ;;   (:a :href "/impressum" "Impressum"))
      (:a :href "http://common-lisp.net"
          (:img :src (make-webapp-public-file-uri "images/footer/lisp-lizard.png")
                :alt "Public Projects for Common Lisp"))
      (str "&nbsp;")
      (:a :href "http://www.lisp.org"
          (:img :src (make-webapp-public-file-uri "images/footer/lambda-lisp.png")
                :alt "This page is made with Common Lisp."))
      (str "&nbsp;")
      (:a :href "http://www.catb.org/hacker-emblem/"
          (:img :src (make-webapp-public-file-uri "images/footer/hacker.png")
                :alt "Thou hacking shall be beautiful.")))))

;;; Website Header

(let ((headings (make-hash-table :test #'equal)))
  (defun website-heading (key)
    (multiple-value-bind (value present) (gethash key headings)
      (if present
          value
          (string-capitalize key))))

  (defun (setf website-heading) (value key)
    (setf (gethash key headings) value))

  nil)

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
  (make-widget (list
                (make-instance 'static-selector
                               :panes (list (cons "home"   (make-home-page))
                                            (cons "math"   (make-math-page))
                                            (cons "poetry" (make-poems-page))
                                            (cons "fun"    (make-fun-page)))))))

;;; Fun

(defun make-fun-page ()
  (make-widget (f_%
                 (with-html
                   (:h1 "Bla"))
                 (render-link (f_% (print "Woo!")) "Erm?"))))

;;; Home

(setf (website-heading "home") *website-name*)

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

(setf (website-heading "poetry") "Poetry")

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

(setf (website-heading "pensieve") "Daniel's Pensieve")

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
