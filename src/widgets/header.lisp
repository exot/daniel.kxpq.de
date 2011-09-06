
(in-package :website)

;;;

(defwidget header (on-demand-selector)
  ((default-title :accessor header-default-title
                  :initarg :default-title
                  :type string
                  :initform "")
   (title-list :accessor header-title-list
               :type list
               :initarg :title-list)))

(defmethod initialize-instance :after ((header header) &key &allow-other-keys)
  (setf (on-demand-lookup-function header)
        (lambda (header tokens)
          (let ((title nil))
            (if (null tokens)
                (setf title (header-default-title header))
                (setf title (or (second (find-if (f_ (equalp (first _) (first tokens)))
                                                 (header-title-list header)))
                                (header-default-title header))))
            (values (make-widget (f_% (with-html
                                        (:div :style "float:right;padding-right:3em"
                                              (:a :href "http://weblocks.viridian-project.de/"
                                                  (:img :src (make-webapp-public-file-uri "images/weblocks-alien-small.png")
                                                        :alt "Made with Weblocks"
                                                        :style "vertical-align:middle")))
                                        (:h1 :style "text-align:left;padding:0 0 2em 3em;"
                                             (str title)))))
                    nil
                    tokens
                    :no-cache)))))

;;;

nil