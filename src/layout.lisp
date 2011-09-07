
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

;;;

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

;;;

(defun make-home-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (make-widget
                                (f_% (with-html
                                       (when *initial-poem*
                                         (render-widget (make-instance 'standard-poem-widget
                                                                       :poem *initial-poem*)))
                                       (:p "This is " (:a :href (make-webapp-uri "/home/personal")
                                                          "Daniel's")
                                           " homepage.")
                                       (to-come)))))
                         (cons "personal"
                               (make-widget
                                (f_% (with-html
                                       (:p "My name is Daniel Borchmann and I'm a PhD math student at the "
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
                                                 (:a :href "http://www.dante.de" "LaTeX")
                                                 " and other languages."))
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
                                                 "0x25a1906f"))))))))))

;;;

(defun make-math-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "main"
                               (make-widget
                                (f_% (with-html
                                       (:p (:a :href (make-webapp-uri "/math/algebra/") "Algebra")
                                           ", "
                                           (:a :href (make-webapp-uri "/math/fca/") "Formal Concept Analysis"))
                                       (to-come)))))
                         (cons "algebra"
                               (make-widget
                                (f_% (with-html
                                       (:h2 "Algebra")))))
                         (cons "fca"
                               (make-widget
                                (f_% (with-html
                                       (:h2 "Formal Concept Analysis")
                                       (:p "Formal Concept Analysis (FCA) is a mathematical theory
                                       developed in the early eighties to restructure lattice theory
                                       by Prof. Rudolf Wille at the TU Darmstadt.  Since then, it has
                                       developed into a powerful theory for conceptual structuring
                                       and knowledge acquisition, with connections to other knowledge
                                       representation formalisms like Description Logics.")
                                       (:h3 "Conferences")
                                       (to-come)
                                       (:h3 "Useful Resources")
                                       (to-come))))))))

;;;

(defun make-conexp-clj-page ()
  (html-from-markdown "conexp-clj.md"))

;;;

(defun make-poems-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "list" (make-poem-list-page))
                         (cons "edit" (make-instance 'login-maybe
                                                     :child-widget (f_%       
                                                                     (make-poem-edit-page))
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

;;;

(defun make-common-lisp-page ()
  (html-from-markdown "lisp.md"))

;;;

nil