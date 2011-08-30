
(in-package :website)

(export '(*default-short-text-length*
          short-text
          short-text-presentation))

;;;

(defparameter *default-short-text-length* 20
  "Default length to which text is trimmed when represented as short text.")

(defclass short-text-presentation (text-presentation)
  ((max-length :initform      *default-short-text-length*
               :initarg       :max-length
               :accessor      short-text-max-length
               :documentation "Maximal length of short-text representation."))
  (:documentation "Presents a large amount of text as short snippet."))

;;;

(defmethod render-view-field-value (value (presentation short-text-presentation)
                                    field view widget obj
                                    &rest args)
  (let* ((value (apply #'print-view-field-value value presentation field view widget obj args)))
    (with-html
      (:p :class "value text short-text"
          (if (> (length value) (short-text-max-length presentation))
              (str (concatenate 'string
                                (subseq value 0 (- (short-text-max-length presentation) 3))
                                "..."))
              (str value))))))

;;;

nil
