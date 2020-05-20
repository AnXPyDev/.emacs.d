(lightning-load-module "setup-package")

(defmacro lightning-use-package-wrapper(name &rest args)
  `(use-package ,name
     :ensure ,(plist-get args :require)))

(defmacro lightning-straight-use-package-wrapper(name &rest args)
  `(straight-use-package ',name))

(defmacro lightning-default-pkg(package-name &rest args)
  `(progn
     ,(plist-get args :config-before)
     (unless (package-installed-p ',package-name)
       (package-install ',package-name))
     (when ,(plist-get args :require)
       (require ',(or (plist-get args :require-symbol) package-name)))
     ,(plist-get args :config-after)))

(defalias 'pkg 'lightning-default-pkg)

(defun lightning-replace-package-manager(name)
  "Loads either use-package or straight as a package manager and aliases them to lightning-use-package"
  (when (string-equal name "use-package")
    (lightning-load-module "install-use-package")
    (defalias 'pkg 'lightning-use-package-wrapper))
  (when (string-equal name "straight")
    (lightning-load-module "bootstrap-straight")
    (defalias 'pkg 'lightning-straight-use-package-wrapper))
  (when (string-equal name "lightning")
    (defalias 'pkg 'lightning-default-pkg)))
