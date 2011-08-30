
(in-package :website)

;;;

(defstore *store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :website)))

;;;

nil
