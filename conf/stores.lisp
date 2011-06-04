
(in-package :website)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.

(defstore *website-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :website)))

