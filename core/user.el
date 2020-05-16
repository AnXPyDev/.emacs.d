(defvar lightning-user-name "anon"
  "Name of user")

(defvar lightning-user-indent-offset 2
  "Number of spaces that represent one indentation level")

(defvar lightning-user-indent-use-spaces t
  "Uses spaces instead of tabs when t")

(defvar lightning-user-default-cursor 'box)

(defun lightning-apply-user-variables()
  "Applies variables prefixed with lightning-user to vanilla emacs variables"
  (interactive)
  (setq-default indent-tabs-mode (not lightning-user-indent-use-spaces))
  (setq-default tab-width lightning-user-indent-offset)
  (setq-default cursor-type lightning-user-default-cursor-type))
