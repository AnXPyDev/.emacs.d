(defvar lightning-modal-default-mode nil
  "Mode used by default in buffers with a major mode that is not paired with another mode and is not ignored")
(defvar lightning-modal-modes '()
  "List of modal modes")
(defvar lightning-modal-major-mode-pairs '()
  "Associative list of major modes with their default modal modes")
(defvar lightning-modal-ignored-major-modes '()
  "List of ignored major modes")

(defun lightning-modal-extern-mode-name(name)
  (intern (concat "lightning-modal-" (symbol-name name) "-mode")))

(defun lightning-modal-raise-function-name(name)
  (intern (concat "lightning-modal-raise-" (symbol-name name) "-mode")))

(defun lightning-modal-extern-hook-name(name)
  (intern (concat (symbol-name (lightning-modal-extern-mode-name name)) "-hook")))

(defmacro lightning-modal-define-mode(name &rest args)
  `(progn
     (push ',name lightning-modal-modes)
     (define-minor-mode ,(lightning-modal-extern-mode-name name)
       ,(plist-get args :doc)
       ,nil
       ,(plist-get args :lighter)
       ,(plist-get args :keymap)
       (if ,(lightning-modal-extern-mode-name name)
         ,(plist-get args :on-enable)
         ,(plist-get args :on-disable)))
     (defun ,(lightning-modal-raise-function-name name) nil
       (interactive)
       (lightning-modal-raise-mode ,name))))

(defmacro lightning-modal-add-hook(name function)
  `(add-hook ',(lightning-modal-extern-hook-name name) ,function))

(defmacro lightning-modal-remove-hook(name function)
  `(remove-hook ',(lightning-modal-extern-hook-name name) ,function))
(defmacro lightning-modal-pair-major-mode(major-mode-name name)
  `(push (cons ',major-mode-name ',name) lightning-modal-major-mode-pairs))

(defmacro lightning-modal-ignore-major-mode(major-mode-name)
  `(push ',major-mode-name lightning-modal-ignored-major-modes))

(defmacro lightning-modal-set-default-mode(name)
  `(setq lightning-modal-default-mode ',name))

(defmacro lightning-modal-disable-mode(name)
  `(,(lightning-modal-extern-mode-name name) 0))

(defmacro lightning-modal-enable-mode(name)
  `(,(lightning-modal-extern-mode-name name) 1))

(defun lightning-modal-disable-all-modes()
  (interactive)
  (dolist (mode lightning-modal-modes)
    (eval `(lightning-modal-disable-mode ,mode))))

(defmacro lightning-modal-raise-mode(name)
  `(progn
     (dolist (mode lightning-modal-modes)
       (eval `(lightning-modal-disable-mode ,mode)))
     (lightning-modal-enable-mode ,name)))

(defun lightning-modal-raise-default-mode()
  (interactive)
  (let ((default-mode (catch 'default-mode
    (if (not (or (member major-mode lightning-modal-ignored-major-modes) (minibufferp)))
	(throw 'default-mode (or (cdr (assoc major-mode lightning-modal-major-mode-pairs)) lightning-modal-default-mode))
      (throw 'default-mode nil)))))
    (if default-mode
	(eval `(lightning-modal-raise-mode ,default-mode))
      (lightning-modal-disable-all-modes))))


(define-minor-mode lightning-modal-mode "Minor mode that manages lightning-modal modes" nil " Lightning-Modal" nil
  (when lightning-modal-mode
    (lightning-modal-raise-default-mode)))

(define-globalized-minor-mode lightning-modal-global-mode lightning-modal-mode (lambda() (lightning-modal-mode 1)))
