;; Keep init.el clear
(setq custom-file (concat user-emacs-directory "custom.el"))

(defvar lightning-emacs-directory user-emacs-directory
  "Directory containing emacs directory, set to user-emacs-directory by default")

(defvar lightning-core-directory (concat lightning-emacs-directory "core/")
  "Directory containing lightining core, set to core in lightning-emacs-directory by default")

(defvar lightning-modules-directory (concat lightning-emacs-directory "modules/")
  "Directory containing lightining modules, set to modules in lightning-emacs-directory by default")

(defvar lightning-config-directory (concat lightning-emacs-directory "config/")
  "Directory containing user config, set to config in lightning-emacs-directory by default")

(defvar lightning-init-file (concat lightning-core-directory "init")
  "Core file of lightining, core in lightning-core-directory by default")

(defvar lightning-config-init-file (concat lightning-config-directory "init")
  "User config file, init in lightning-config-directory by default")

(defun lightning-reload-core()
  (interactive)
  (load lightning-init-file))

(defun lightning-load-module(name)
  (load (concat lightning-modules-directory name)))

(defun lightning-reload-config()
  (interactive)
  (load lightning-config-init-file))

(defun lightning-load-config(name)
  (interactive)
  (load (concat lightning-config-directory name)))

(lightning-reload-config)
