
(in-package :website)

;;;

(defclass poem ()
  ((id)
   (title :accessor poem-title
          :initarg :title
          :initform ""
          :type (or null string))
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

;;;

(defun find-poem-by-predicate (predicate)
  "Returns the first poem found that satisfies predicate.  Return nil if none such poem exists."
  (find-if predicate (find-persistent-objects *store* 'poem)))

(defun find-poem-by-title (title)
  "Returns first poem found having the given title, returning nil if no such poem exists."
  (find-poem-by-predicate (f_ (equalp title (poem-title _)))))

(defun find-poem-by-start (start)
  "Returns first poem found that starts with start, nil otherwise."
  (let ((len (length start)))
    (find-poem-by-predicate (f_ (equalp start (subseq (poem-body _) 0 len))))))

;;;

nil
