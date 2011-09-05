;;; -*- mode: lisp -*-

(defpackage #:website-asd
  (:use :cl :asdf))

(in-package :website-asd)

(defsystem website
  :name "website"
  :version "0.0.1"
  :maintainer "Daniel"
  :author "Daniel"
  :licence "None"
  :description "website"
  :depends-on (:weblocks :cl-prevalence)
  :components ((:file "website")
               (:module conf
                :components ((:file "stores"))
                :depends-on ("website"))
               (:module src
                :depends-on (conf "website")
                :components ((:file "init-session"
                              :depends-on (models views "layout"))
                             (:file "layout"
                              :depends-on (widgets))
                             (:module widgets
                              :components ((:file "poem")
                                           (:file "login-maybe")))
                             (:module models
                              :components ((:file "user")
                                           (:file "poem")))
                             (:module views
                              :components ((:file "user")
                                           (:file "poem"
                                            :depends-on (types))
                                           (:module types
                                            :components ((:module presentations
                                                          :components ((:file "short-text"))))))
                              :depends-on (models))))))
