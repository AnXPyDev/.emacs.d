(lightning-load-module "package-manager")

(lightning-pkg dashboard :require t
  :config-after
  (progn
    (dashboard-setup-startup-hook)))
