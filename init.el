(setq custom-file (concat user-emacs-directory "custom"))

(defvar lightning-config-directory (concat user-emacs-directory "config/")
  "Directory containing user config, set to .lightning.d in parent of user-emacs-directory by default, can be modified at startup with --lightning-config-directory")

(defvar lightning-modules-directory (concat user-emacs-directory "modules/")
  "Directory containing lightining modules, set to modules in lightning-emacs-directory by default")

(defvar lightning-suppress-config-errors t
  "Ignores errors invoked while loading a config file with the lightning-load-config function")

(defvar lightning-rethrow-error nil
  "Lightning error handler rethrows error when set to t")

(defun lightning-handle-error(err &rest args)
  "Handles errors"
  (message (plist-get args :message))
  (if (or (plist-get args :rethrow-error) lightning-rethrow-error)
      (signal (car err) (cdr err))
    (message (error-message-string err))))

(defun lightning-load(name &rest args)
  "Loads a file using built-in load but can suppress errors if :suppress-errors is t"
  (condition-case err
      (load (concat (plist-get args :directory) name))
    (error
     (lightning-handle-error
      err
      :message (concat "[ERR] An error was encountered when loading " name)
      :rethrow-error (not (plist-get args :suppress-errors))))))

(defvar lightning-loaded-modules '()
  "List of loaded lightning modules")

(defun lightning-load-module(name &rest args)
  "Loads a module located in lightning-modules-directory if it's not already loaded, you can override this check with :force t"
  (let ((already-loaded (member name lightning-loaded-modules)))
    (if (or (not already-loaded) (plist-get args :force))
	(progn
	  (when (lightning-load name :directory lightning-modules-directory :suppress-errors nil)
	    (unless already-loaded (push name lightning-loaded-modules)) t))
      t)))

(defun lightning-reload-modules()
  "Reloads all previously loaded modules"
  (interactive)
  (dolist (module lightning-loaded-modules)
    (lightning-load-module module :force t)))

(defun lightning-load-config(name)
  "Loads a config file located in lightning-config-directory"
  (lightning-load name :directory lightning-config-directory :suppress-errors lightning-suppress-config-errors))

(defun lightning-reload-config()
  "Reloads user config"
  (interactive)
  (lightning-load-config "init"))

(push
  (cons "--lightning-no-config" 'ignore)
  command-switch-alist)

(unless (member "--lightning-no-config" command-line-args)
  (lightning-reload-config))
