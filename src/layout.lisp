
(in-package :website)

;;;

(defun make-home-page ()
  (make-instance 'static-selector
                 :panes (list
                         (cons "home"
                               (f_% (with-html
                                      (:h2 "At Home")
                                      (:p "This is " (:a :href (make-webapp-uri "/home/personal")
                                                         "Daniel's")
                                          " homepage, made with Weblocks!"))))
                         (cons "personal"
                               (f_% (with-html
                                      (:h2 "Daniel Borchmann")
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
                 :panes `(("calculus" . ,(f_% (with-html
                                                (:h2 "Calculus")
                                                (:a :href (make-webapp-uri "/math/algebra/")
                                                    "Algebra"))))
                          ("algebra"  . ,(f_% (with-html
                                                (:h2 "Algebra")
                                                (:a :href (make-webapp-uri "/math/calculus/")
                                                    "Calculus")))))))

;;;

(defun make-conexp-clj-page ()
  (f_% (redirect "http://www.math.tu-dresden.de/~borch/conexp-clj/")))

;;;

(defun make-poems-page ()
  (f_% (with-html
         (:h2 "Poems"))))

;;;

(defun make-common-lisp-page ()
  (f_% (with-html
         (:h2 "Common Lisp the Language")
         (:a :href (make-webapp-public-file-uri "init-session.lisp")
             "Link")
         (:pre "(+ 1 2 3)"))))

;;;

nil