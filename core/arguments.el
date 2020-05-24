(push
  (cons "--lightning-no-config" 'ignore)
  command-switch-alist)

(unless (member "--lightning-no-config" command-line-args)
  (lightning-reload-config))
