(lightning-load-module "setup-package")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
