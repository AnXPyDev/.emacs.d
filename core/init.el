(defun parent-directory(dir)
  (file-name-directory (directory-file-name dir)))

(defvar lightning-core-directory (concat user-emacs-directory "core/")
  "Directory containing lightining core, set to core in lightning-emacs-directory by default")

(defvar lightning-modules-directory (concat user-emacs-directory "modules/")
  "Directory containing lightining modules, set to modules in lightning-emacs-directory by default")

(defvar lightning-config-directory (concat (parent-directory user-emacs-directory) ".lightning.d/")
  "Directory containing user config, set to .lightning.d in parent of user-emacs-directory by default, can be modified at startup with --lightning-config-directory")

(defvar lightning-suppress-config-errors t
  "Ignores errors invoked while loading a config file with the lightning-load-config function")

(defun lightning-load(name &rest args)
  "Loads a file using built-in load but can suppress errors if :suppress-errors is t"
  (condition-case err
      (load (concat (plist-get args :directory) name))
    (error
     (lightning-handle-error
      err
      :message (concat "[ERR] An error was encountered when loading " name)
      :rethrow-error (not (plist-get args :suppress-errors))))))

(defun lightning-reload-core()
  "Reloads lightning"
  (interactive)
  (lightning-load "init" :directory lightning-core-directory :suppress-errors nil))

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

(lightning-load "error" :directory lightning-core-directory)
(lightning-load "user" :directory lightning-core-directory)
(lightning-load "arguments" :directory lightning-core-directory)
