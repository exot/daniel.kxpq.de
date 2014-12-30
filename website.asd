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
  :depends-on (:weblocks-stores
               :weblocks-memory
               :weblocks
               :cl-prevalence
               :trivial-shell)
  :components ((:file "website")
               (:module conf
                :depends-on ("website")
                :components ((:file "stores")))
               (:module src
                :depends-on (conf "website")
                :components ((:file "util")
                             (:module models
                              :components ((:file "user")
                                           (:file "poem")))
                             (:module views
                              :depends-on (models)
                              :components ((:file "user")
                                           (:module types
                                            :components ((:module presentations
                                                          :components ((:file "short-text")))))
                                           (:file "poem"
                                            :depends-on (types))))
                             (:module widgets
                              :depends-on (models)
                              :components ((:file "poem")
                                           (:file "login-maybe")))
                             (:file "layout"
                              :depends-on (models views widgets))))))
