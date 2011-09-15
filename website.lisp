
(defpackage #:website
  (:use :cl
        :weblocks
        :f-underscore
        :anaphora
        :cl-who)
  (:import-from :hunchentoot
                #:header-in
		#:set-cookie
                #:set-cookie*
                #:cookie-in
		#:user-agent
                #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :website)

(export '(start-website stop-website))

;;; A macro that generates a class or this webapp

(defwebapp website
    :prefix "/"
    :name "Daniel's Playground"
    :init-user-session 'website::init-user-session
    :ignore-default-dependencies nil
    :public-files-path #P"/home/borch/Documents/lang/lisp/cl/source/website/pub/"
    :autostart nil
    :debug t
    )

;;; Top level start & stop scripts

(defun start-website (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks :port 52341 args)
  (start-webapp 'website))

(defun stop-website ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'website)
  (stop-weblocks))

;;;

nil