(setq lmodal-default-mode nil)
(setq lmodal-modes '())
(setq lmodal-major-mode-pairs '())
(setq lmodal-ignored-major-modes '())
(setq-default lmodal-history '())

(defun lmodal-extern-mode-name(name)
  (intern (concat "lmodal/" (symbol-name name))))

(defun lmodal-extern-hook-name(name)
  (intern (concat (symbol-name (lmodal-extern-mode-name name)) "-hook")))

(defmacro lmodal-define-mode(name &rest args)
  `(progn
     (push ',name lmodal-modes)
     (define-minor-mode ,(lmodal-extern-mode-name name)
       ,(plist-get args :doc)
       ,nil
       ,(plist-get args :lighter)
       ,(plist-get args :keymap)
       (if ,(lmodal-extern-mode-name name)
     ,(plist-get args :on-enable)
   ,(plist-get args :on-disable)))))

(defmacro lmodal-add-hook(name function)
  `(add-hook ',(lmodal-extern-hook-name name) ,function))

(defmacro lmodal-remove-hook(name function)
  `(remove-hook ',(lmodal-extern-hook-name name) ,function))
(defmacro lmodal-pair-major-mode(major-mode-name name)
  `(push (cons ',major-mode-name ',name) lmodal-major-mode-pairs))

(defmacro lmodal-ignore-major-mode(major-mode-name)
  `(push ',major-mode-name lmodal-ignored-major-modes))

(defmacro lmodal-set-default-mode(name)
  `(setq lmodal-default-mode ',name))

(defmacro lmodal-disable-mode(name)
  `(,(lmodal-extern-mode-name name) 0))

(defmacro lmodal-enable-mode(name)
  `(,(lmodal-extern-mode-name name) 1))

(defun lmodal-disable-all-modes()
  (dolist (mode lmodal-modes)
    (eval `(lmodal-disable-mode ,mode))))

(defmacro lmodal-raise-mode(name)
  `(progn
     (dolist (mode lmodal-modes)
       (eval `(lmodal-disable-mode ,mode)))
     (lmodal-enable-mode ,name)))

(defun lmodal-raise-default-mode()
  (let ((default-mode (catch 'default-mode
    (if (not (or (member major-mode lmodal-ignored-major-modes) (minibufferp)))
	(throw 'default-mode (or (cdr (assoc major-mode lmodal-major-mode-pairs)) lmodal-default-mode))
      (throw 'default-mode nil)))))
    (if default-mode
	(eval `(lmodal-raise-mode ,default-mode))
      (lmodal-disable-all-modes))))


(define-minor-mode lmodal-mode "Minor mode that manages lmodal modes" nil " Lmodal" nil
  (when lmodal-mode
    (lmodal-raise-default-mode)))

(define-globalized-minor-mode lmodal-global-mode lmodal-mode (lambda() (lmodal-mode 1)))

(provide 'lmodal)
