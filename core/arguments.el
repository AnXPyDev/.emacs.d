(push
  (cons "--lightning-load-config" (lambda (arg) (interactive) (lightning-reload-config)))
  command-switch-alist)
