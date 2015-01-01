(defpackage #:website
  (:use :cl
        :weblocks-stores
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

;;;

(defparameter *website-name* "Reaching for the Stars")

(defwebapp website
  :prefix "/"
  :name *website-name*
  :init-user-session 'website::init-user-session
  :ignore-default-dependencies nil
  :public-files-path #P"./pub"       ; Note: run the website in the directory of this file
  :autostart nil
  :html-indent-p t
  :dependencies '((:script "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                  (:script "mathjax")
                  (:stylesheet "http://fonts.googleapis.com/css?family=Cardo:400,400italic,700&subset=latin")))

;;; Top level start & stop scripts

(defun start-website (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks :port 52341 args)
  (start-webapp 'website)
  (setf (hunchentoot:acceptor-access-log-destination *weblocks-server*) #p"access.log"
        (hunchentoot:acceptor-message-log-destination *weblocks-server*) #p"message.log"))

(defun stop-website ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'website)
  (stop-weblocks))

;;;

nil
