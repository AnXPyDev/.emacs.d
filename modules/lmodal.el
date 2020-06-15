(defvar lmodal-default-mode nil
  "Mode used by default in buffers with a major mode that is not paired with another mode and is not ignored")
(defvar lmodal-all-modes '())
(defvar lmodal-modes '()
  "List of modal modes that aren't paired with a major mode")
(defvar lmodal-mm-modes-alist '()
  "Associative list of major modes with their available modal modes")
(defvar lmodal-mm-default-mode-alist '()
  "Associative list of major modes with their default modal modes")
(defvar lmodal-ignored-major-modes '()
  "List of ignored major modes")

(defun lmodal-extern-mode-name(name &optional major-mode)
  (let
    (( available-modes (alist-get major-mode lmodal-mm-modes-alist) ))
    (if (and available-modes (member name available-modes))
      (intern (concat "lmodal-" (symbol-name major-mode) "-" (symbol-name name) "-mode" ))
      (intern (concat "lmodal-" (symbol-name name) "-mode")))
    )
  )

(defun lmodal-raise-function-name(name)
  (intern (concat "lmodal-raise-" (symbol-name name) "-mode")))

(defun lmodal-register-mode(name &optional major-mode)
  (unless (member name lmodal-all-modes)
    (push name lmodal-all-modes))
  (if major-mode
    (progn
      (let
        (( available-modes (alist-get major-mode lmodal-mm-modes-alist) ))
        (unless available-modes
          (push (cons major-mode '()) lmodal-mm-modes-alist))
        )
      (push name (alist-get major-mode lmodal-mm-modes-alist))
      )
    (push name lmodal-modes)
    ))

(defmacro lmodal-define-mode(name &rest args)
  (lmodal-register-mode name (plist-get args :major-mode))
  (setq extern-mode-name (lmodal-extern-mode-name name (plist-get args :major-mode)))
  `(progn
     (define-minor-mode ,extern-mode-name
       ,(plist-get args :doc)
       ,nil
       ,(plist-get args :lighter)
       ,(plist-get args :keymap)
       (if ,extern-mode-name
         (progn
           ,(plist-get args :on-enable)
           ,(when (plist-member args :cursor)
              `(setq cursor-type ,(plist-get args :cursor)))
           )
         ,(plist-get args :on-disable)))
     (defun ,(lmodal-raise-function-name name) nil
       (interactive)
       (lmodal-raise-mode ,name))))

(defmacro lmodal-ignore-major-mode(major-mode-name)
  `(push ',major-mode-name lmodal-ignored-major-modes))

(defmacro lmodal-set-default-mode(name &optional major-mode)
  (if major-mode
    `(push (cons ',major-mode ',name) lmodal-mm-default-mode-alist)
    `(setq lmodal-default-mode ',name)))

(defmacro lmodal-disable-mode(name &optional major-mode)
  (when name
    `(,(lmodal-extern-mode-name name major-mode) 0)))

(defmacro lmodal-enable-mode(name &optional major-mode)
  (when name
    `(,(lmodal-extern-mode-name name major-mode) 1)))

(defun lmodal-disable-all-modes()
  (interactive)
  (dolist (mode lmodal-all-modes)
    (eval `(lmodal-disable-mode ,mode ,major-mode))))

(defun lmodal-get-default-mode(&optional major-mode)
  (interactive)
  (or (alist-get major-mode lmodal-mm-default-mode-alist) lmodal-default-mode))
 

(defmacro lmodal-raise-mode(&optional name)
  (if name
    `(progn
       (lmodal-disable-all-modes)
       (eval `(lmodal-enable-mode ,',name ,major-mode)))
    (progn
      (lmodal-disable-all-modes)
      (eval `(lmodal-enable-mode ,(lmodal-get-default-mode major-mode) ,major-mode)))))


(defun lmodal-raise-default-mode()
  (interactive)
  (lmodal-disable-all-modes)
  (eval `(lmodal-enable-mode ,(lmodal-get-default-mode major-mode) ,major-mode)))
      
(define-minor-mode lmodal-mode "Minor mode that manages lmodal modes" nil " Lmodal" nil
  (when lmodal-mode
    (lmodal-raise-mode)))

(define-globalized-minor-mode lmodal-global-mode lmodal-mode (lambda() (lmodal-mode 1)))
