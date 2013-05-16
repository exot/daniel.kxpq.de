
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

;;;

(defparameter *website-name* "whoami")

(defwebapp website
    :prefix "/"
    :name *website-name*
    :init-user-session 'website::init-user-session
    :ignore-default-dependencies nil
    :public-files-path #P"/home/borch/Documents/lang/lisp/cl/source/website/pub/"
    :autostart nil
    :debug nil
    :html-indent-p t
    :dependencies  '((:script "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                     (:script "mathjax"))
    )

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
