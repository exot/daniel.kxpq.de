
(in-package :website)

;;;

(defmethod render-navigation-menu ((obj navigation) &rest args &key menu-args &allow-other-keys)
  (declare (ignore args))
  (apply #'render-menu (navigation-menu-items obj)
         :base (selector-base-uri obj)
         :selected-pane nil
         :header (navigation-header obj)
         :container-id (dom-id obj)
         :empty-message "No navigation entries"
         menu-args))

;;;

(defun make-home-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (f_% (with-html
                                      (:h2 "At Home")
                                      (:p "This is " (:a :href (make-webapp-uri "/home/personal")
                                                         "Daniel's")
                                          " homepage.  There is not much here right now,
                                          though."))))
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
                                      (:p "I like programming, math (calculus and algebra), computer
                                           science, Linux and Unix.  Apart from this,
                                           expressionistic literature, "
                                          (:a :href (make-webapp-uri "/poems") "modern poetry")
                                          ", buddhism and japanese animated cartoons from "
                                          (:a :href "http://en.wikipedia.org/wiki/Hayao_Miyazaki"
                                              "Hayao Miyazaki")
                                          " are of my interest.  I also like to play "
                                          (:a :href "http://sensei.xmp.net" "Go")
                                          ".")
                                      (:p (:dl
                                           (:dt "Programming")
                                           (:dd
                                            (:p (:a :href "http://www.lisp.org" "Common Lisp")
                                                ", "
                                                (:a :href "http://www.clojure.org/" "Clojure")
                                                " and "
                                                (:a :href "http://www.schemers.org/" "Scheme")
                                                ", but also "
                                                (:a :href "http://en.wikipedia.org/wiki/C_(programming_language)"
                                                    "C")
                                                ", "
                                                (:a :href "http://www.perl.org" "Perl")
                                                ", "
                                                (:a :href "http://www.dante.de" "LaTeX") " and other
                                                languages."))
                                           (:dt "Other Quirks")
                                           (:dd
                                            (:p "vegetarian, idealist, dreamer, nonsmoker,
                                                 teetotaler, non-coffee-drinker, bearded, Lisp
                                                 jerk (though my programming is not very good)"))))
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
                                            (:img :src (make-webapp-public-file-uri "/images/math/ramanujan.png")))
                                      (:p :style "font-style:italic"
                                          "To come.")
                                      (:h3 "Mathematical Fields of Interest")
                                      (:p (:a :href (make-webapp-uri "/math/algebra/") "Algebra")
                                          ", "
                                          (:a :href (make-webapp-uri "/math/fca/") "Formal Concept Analysis")))))
                         (cons "algebra"
                               (f_% (with-html
                                      (:h2 "Algebra"))))
                         (cons "fca"
                               (f_% (with-html
                                      (:h2 "Formal Concept Analysis")
                                      (:p "Formal Concept Analysis (FCA) is a mathematical theory
                                      developed in the early eighties to restructure lattice theory
                                      by Prof. Rudolf Wille at the TU Darmstadt.  Since then, it has
                                      developed into a powerful theory for conceptual structuring
                                      and knowledge acquisition, with connections to other knowledge
                                      representation formalisms like Description Logics.")
                                      (:p :style "font-style:italic" "More to come.")))))))

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
         (:h2 "The Lisp Family"))))

;;;

nil