
(defpackage #:website
  (:use :cl :weblocks
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :website)

(export '(start-website stop-website))

;; A macro that generates a class or this webapp

(defwebapp website
    :prefix "/"
    :description "website: A new application"
    :init-user-session 'website::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )

;; Top level start & stop scripts

(defun start-website (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks args)
  (start-webapp 'website))

(defun stop-website ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'website)
  (stop-weblocks))

