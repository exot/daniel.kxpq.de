
(in-package :website)

;;; From http://teddyb.org/rlp/tiki-index.php?page=Learning+About+Weblocks

(defwidget login-maybe (login)
  ((child-widget :accessor login-maybe-child-widget
                 :initarg :child-widget
                 :documentation "The widget to render if we are already logged in.  Must be wrapped
                 in (lambda () ...) so that the bits inside can use auth information.  The lambda
                 will be run exactly once.")
   (real-child-widget
    :documentation "Where the result of the child-widget lambda gets put."))
  (:documentation "Render login form only if not logged in."))

(defmethod initialize-instance ((self login-maybe) &key &allow-other-keys)
  (call-next-method)
  (setf (widget-continuation self)
        (lambda (&optional auth)
          (declare (ignore auth))       ;unless you care...
          (mark-dirty self))))

(defmethod render-widget-body ((self login-maybe) &key &allow-other-keys)
  (cond
    ((authenticatedp)
     (render-widget (slot-value self 'real-child-widget) :inlinep t))
    (t (call-next-method))))

(defun check-login (login-widget credentials-obj)
  "Check the user's login credentials"
  (declare (ignore credentials-obj))
  (cond
    ; For now, we accept anything
    (t
      (when (login-maybe-child-widget login-widget)
        (setf (slot-value login-widget 'real-child-widget)
              (funcall (login-maybe-child-widget login-widget)))
        (setf (login-maybe-child-widget login-widget) nil))
      t)))

;;;

nil