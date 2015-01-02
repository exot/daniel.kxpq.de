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

(defwebapp website
  :prefix "/"
  :name "Reaching for the Stars"
  :init-user-session 'website::init-user-session
  :ignore-default-dependencies nil
  :public-files-path (merge-pathnames #P"./pub" (asdf-system-directory :website))
  :autostart nil
  :html-indent-p t
  :dependencies '((:script "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                  (:script "mathjax")
                  (:stylesheet "http://fonts.googleapis.com/css?family=Cardo:400,400italic,700&subset=latin")))

(defstore *store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
                   (asdf-system-directory :website)))

;;; Top level start & stop scripts

(defparameter *starting-time* (local-time:now))

(defun start-website (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks :port 52341 args)
  (start-webapp 'website)
  (setf *starting-time* (local-time:now))
  (setf (hunchentoot:acceptor-access-log-destination *weblocks-server*) #p"access.log"
        (hunchentoot:acceptor-message-log-destination *weblocks-server*) #p"message.log"))

(defun stop-website ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'website)
  (stop-weblocks))

(defun restart-website (&rest args)
  "Stops and starts the website, passing ARGS to START-WEBSITE."
  (stop-website)
  (apply #'start-website args))

;;;

nil
