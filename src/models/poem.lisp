
(in-package :website)

;;;

(defclass poem ()
  ((id)
   (title :accessor poem-title
          :initarg :title
          :initform ""
          :type string)
   (author :accessor poem-author
           :initarg :author
           :initform ""
           :type string)
   (body :accessor poem-body
         :initarg :body
         :initform ""
         :type string)
   (source :accessor poem-source
           :initarg :source
           :initform nil
           :type (or null string))))

(defun find-poem-by-title (title)
  "Returns first poem found having the given title, returning nil if no such poem exists."
  (find-if (f_ (equalp title (poem-title _)))
           (find-persistent-objects *store* 'poem)))

;;;

nil
