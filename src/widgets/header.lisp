
(in-package :website)

;;;

(defwidget header ()
  ((title :accessor header-title
          :initarg :title
          :type string)))

(defmethod render-widget ((header header) &key &allow-other-keys)
  (with-html
    (:div :style "float:right;padding-right:3em"
          (:a :href "http://weblocks.viridian-project.de/"
              (:img :src (make-webapp-public-file-uri "images/weblocks-alien-small.png")
                    :alt "Made with Weblocks"
                    :style "vertical-align:middle")))
    (:h1 :style "text-align:left;padding:0 0 2em 3em;"
         (str (header-title header)))))

;;;

nil