(defpackage #:website
  (:use :cl
        :weblocks-stores
        :weblocks
        :f-underscore
        :anaphora
        :cl-who)
  (:export :start-website
           :stop-website
           :restart-website)
  (:documentation
   "A web application based on Weblocks."))

(in-package :website)

;;;

(defvar *website-dir* (asdf-system-directory :website)
  "Directory where all the files reside that we need.")

(defwebapp website
  :prefix "/"
  :name "Reaching for the Stars"
  :init-user-session 'website::init-user-session
  :public-files-path (merge-pathnames #P"./pub/" *website-dir*)
  :autostart nil
  :html-indent-p t
  :ignore-default-dependencies t
  :dependencies '((:stylesheet "main")
                  (:stylesheet "layout")
                  (:stylesheet "http://fonts.googleapis.com/css?family=Cardo:400,400italic,700&subset=latin")
                  (:script "prototype")
                  (:script "weblocks")
                  (:script "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                  (:script "mathjax")))

(defstore *store* :prevalence
  (merge-pathnames #P"./data/" *website-dir*))

;;; Top level start & stop scripts

(defvar *starting-time* (local-time:now)
  "Time when the website server had been started.")

(defvar *message-log* (merge-pathnames #p"message.log" *website-dir*)
  "File where Hunchentoot's messages go.")
(defvar *access-log* (merge-pathnames #p"access.log" *website-dir*)
  "File where website access is logged.")

(defun start-website (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks :port 52341 args)
  (start-webapp 'website)
  (setf *starting-time* (local-time:now))
  (setf (hunchentoot:acceptor-access-log-destination *weblocks-server*) *access-log*
        (hunchentoot:acceptor-message-log-destination *weblocks-server*) *message-log*))

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
