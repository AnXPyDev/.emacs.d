;; Disable annoying sounds
(setq ring-bell-function 'ignore)

;; Disable useless UI elements
(defun lightning-sensible-defaults-clean-up-ui()
  "Disables tool-bar, menu-bar and scroll-bar"
  (interactive)
  (ignore-errors (tool-bar-mode 0))
  (ignore-errors (menu-bar-mode 0))
  (ignore-errors (scroll-bar-mode 0)))

(add-hook 'after-init-hook 'lightning-sensible-defaults-clean-up-ui)

(lightning-sensible-defaults-clean-up-ui)

;; Disable autosaves and backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Use y-n instead of yes-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Treat all themes as safe
(setq custom-safe-themes t)

;; Fix lisp indenting
(setq lisp-indent-offset lightning-user-indent-offset)
