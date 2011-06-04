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
  :depends-on (:weblocks)
  :components ((:file "website")
               (:module conf
                        :components ((:file "stores"))
                        :depends-on ("website"))
               (:module src
                        :components ((:file "init-session"))
                        :depends-on ("website" conf))))

