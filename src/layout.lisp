
(in-package :website)

;;;

(defun make-home-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (f_% (with-html
                                      (:h2 "At Home")
                                      (:p "This is " (:a :href (make-webapp-uri "/home/personal")
                                                         "Daniel's")
                                          " homepage, made with Weblocks!  There is not much here
                                          right now, though."))))
                         (cons "personal"
                               (f_% (with-html
                                      (:h2 "Daniel Borchmann")
                                      (:p "I'm a PhD math student at the "
                                          (:a :href "http://www.math.tu-dresden.de"
                                              "Technische Universität Dresden")
                                          " under supervision of "
                                          (:a :href "http://tu-dresden.de/Members/bernhard.ganter"
                                              "Prof. Bernhard Ganter")
                                          ".  The main focus of my work lies on "
                                          (:a :href (make-webapp-uri "/math/fca/")
                                              "Formal Concept Analysis")
                                          " with special treatment of Association Rules in a
                                          Description Logic setting.")
                                      (:h3 "Personal Quirks")
                                      (:p "To come (oh, well, they are already there, of course, but you understand...)")
                                      (:h3 "Public Keys")
                                      (:p "Here are two public keys, for if you want to securely
                                      communicate with me.  I use the main public key only from my
                                      personal computers, so it can be regarded as more secure,
                                      however reply times might be longer.  Use the external public
                                      key if this is a problem.")
                                      (:dl
                                       (:dt "Main Public Key")
                                       (:dd (:a :href (make-webapp-public-file-uri "public.key")
                                                "0xa1098f58"))
                                       (:dt "External Public Key")
                                       (:dd (:a :href (make-webapp-public-file-uri "extern-public.key")
                                                "0x25a1906f")))))))))

;;;

(defun make-math-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (f_% (with-html
                                      (:h2 "On Math")
                                      (:div :style "text-align:center"
                                            (:img :src (make-webapp-public-file-uri "/images/math/ramanujan.png"))))))
                         (cons "algebra"
                               (f_% (with-html
                                      (:h2 "Algebra"))))
                         (cons "fca"
                               (f_% (with-html
                                      (:h2 "Formal Concept Analysis")
                                      (:p "to come")))))))

;;;

(defun make-conexp-clj-page ()
  (f_% (redirect "http://www.math.tu-dresden.de/~borch/conexp-clj/")))

;;;

(defun make-poems-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "list"
                               (make-instance 'widget
                                              :children
                                              (list
                                               (make-widget
                                                (f_% (with-html
                                                       (:h2 "Poems")
                                                       (:p (:a :href (make-webapp-uri "/poems/edit")
                                                               "Edit Poems")))))
                                               (make-poem-list-page))))
                         (cons "edit"
                               (make-instance 'widget
                                              :children
                                              (list
                                               (make-widget
                                                (f_% (with-html
                                                       (:h2 "Poems")
                                                       (:p (:a :href (make-webapp-uri "/poems/list")
                                                               "List Poems")))))
                                               (make-poem-edit-page)))))))

(defun make-poem-list-page ()
  (make-instance 'poem-selector
                 :filter (constantly t)
                 :poem-widget-class 'standard-poem-widget))

(defun make-poem-edit-page ()
  (make-instance 'gridedit
                 :name 'poems-grid
                 :drilldown-type :view
                 :data-class 'poem
                 :view 'poem-table-view
                 :item-data-view 'poem-data-view
                 :item-form-view 'poem-form-view))

;;;

(defun make-common-lisp-page ()
  (f_% (with-html
         (:h2 "Common Lisp the Language")
         (:a :href (make-webapp-public-file-uri "init-session.lisp")
             "Link")
         (:pre "(+ 1 2 3)"))))

;;;

nil