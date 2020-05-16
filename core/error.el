(defvar lightning-rethrow-error nil
  "Lightning error handler rethrows error when set to t")

(defun lightning-handle-error(err &rest args)
  "Handles errors"
  (message (plist-get args :message))
  (if (or (plist-get args :rethrow-error) lightning-rethrow-error)
      (signal (car err) (cdr err))
    (message (error-message-string err))))
