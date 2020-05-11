(defun config/path(path)
  (concat user-emacs-directory path))

(defun config/reload()
  (interactive)
  (org-babel-load-file (config/path "config.org")))

(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun package-use(name &rest args)
  (unless (package-installed-p name)
    (package-install name))
  (setq arg-require (plist-get args :require))
  (setq arg-require-name (plist-get args :require-name))
  (if arg-require
      (require name)
    (when arg-require-name
      (require arg-require-name))))

(setq ring-bell-function 'ignore)

(ignore-errors (tool-bar-mode 0))
(ignore-errors (menu-bar-mode 0))
(ignore-errors (scroll-bar-mode 0))
(add-hook 'after-init-hook (lambda() (interactive) (ignore-errors (scroll-bar-mode 0))))

(setq make-backup-files nil)
(setq auto-save-default nil)

(electric-pair-mode 1)

(global-linum-mode 1)
(setq linum-format " %3d ")

(global-visual-line-mode 1)

(setq config/indent-size 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width config/indent-size)

(setq-default scroll-step 1)

(setq auto-revert-verbose nil)
(global-auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq config/default-cursor '(hbar . 2))
(setq config/insert-cursor 'bar)
(setq-default cursor-type config/default-cursor)
(blink-cursor-mode 0)

(setq custom-safe-themes t)

(package-use 'general :require t)

(package-use 'which-key :require t)
(which-key-mode 1)

(package-use 'swiper :require t)

(package-use 'company :require t)

(package-use 'projectile :require t)
(projectile-global-mode 1)

(package-use 'ivy :require t)
(ivy-mode 1)

(package-use 'beacon :require t)
(beacon-mode 1)

(package-use 'highlight-parentheses :require t)
(global-highlight-parentheses-mode)

(package-use 'dashboard :require t)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner (config/path "banner.png"))
(setq dashboard-items '((recents . 5)
      (projects . 5)))
(setq dashboard-banner-logo-title "Welcome to Emacs.")

(package-use 'minor-mode-hack :require t)

(package-use 'avy :require t)

(package-use 'elmacro :require t)
(elmacro-mode 1)

(package-use 'exwm :require t)

(package-use 'undo-tree :require t)
(global-undo-tree-mode)

(package-use 'expand-region :require t)

(package-use 'multiple-cursors :require t)

(package-use 'flycheck :require t)
(global-flycheck-mode t)

(define-fringe-bitmap 'flycheck-fringe-bitmap-rectangle
  (vector #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000
          #b11100000))

(flycheck-define-error-level 'error
  ;;:overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-rectangle
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

(flycheck-define-error-level 'warning
  ;;:overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-rectangle
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)

(flycheck-define-error-level 'info
  ;;:overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-rectangle
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-info)

(setq flycheck-display-errors-function nil)

(package-use 'origami :require t)
(global-origami-mode t)

(package-use 'hlinum :require t)

(package-use 'kaolin-themes)
(package-use 'arc-dark-theme)

(package-use 'lua-mode :require t)

(package-use 'company-lua :require t)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-lua))

(setq lua-indent-level tab-width)

(defun lua-calculate-modifier (modifier)
  (if (= modifier 0)
      0
    lua-indent-level))

(defun lua-calculate-indentation (&optional parse-start)
  (save-excursion
    (let ((continuing-p (lua-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       (lua-calculate-indentation-override)

       (when (lua-forward-line-skip-blanks 'back)
         (let* ((modifier
                 (lua-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) (lua-calculate-modifier modifier))))
       0))))

(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level."
  (let ((indentation-modifier 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      ;; Look for the last block closing token
      (back-to-indentation)
      (if (and (not (lua-comment-or-string-p))
               (looking-at lua-indentation-modifier-regexp)
               (let ((token-info (lua-get-block-token-info (match-string 0))))
                 (and token-info
                      (not (eq 'open (lua-get-token-type token-info))))))
          (when (lua-goto-matching-block-token nil nil 'backward)
            ;; Exception cases: when the start of the line is an assignment,
            ;; go to the start of the assignment instead of the matching item
            (let ((block-start-column (current-column))
                  (block-start-point (point)))
              (if (lua-point-is-after-left-shifter-p)
                  (current-indentation)
                block-start-column)))))))

(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level."
  (let ((indentation-modifier 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      ;; Look for the last block closing token
      (back-to-indentation)
      (if (and (not (lua-comment-or-string-p))
               (looking-at lua-indentation-modifier-regexp)
               (let ((token-info (lua-get-block-token-info (match-string 0))))
                 (and token-info
                      (not (eq 'open (lua-get-token-type token-info))))))
          (when (lua-goto-matching-block-token)
            ;; Exception cases: when the start of the line is an assignment,
            ;; go to the start of the assignment instead of the matching item
            (let ((block-start-column (current-column))
                  (block-start-point (point)))
              (if (lua-point-is-after-left-shifter-p)
                  (current-indentation)
                (current-indentation))))))))

(package-use 'moonscript :require t)

(package-use 'irony :require t)
(package-use 'company-irony :require t)
(package-use 'company-c-headers :require t)

(defun lang-c/add-hook (func-name)
  (add-hook 'c++-mode-hook func-name)
  (add-hook 'c-mode-hook func-name))

(when (not (string-equal system-type "windows-nt"))
  (lang-c/add-hook 'irony-mode))

(defun lang-c/change-checker()
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
  (add-to-list 'flycheck-enabled-checkers 'c/c++-gcc)
  (delete 'c/c++-clang flycheck-enabled-checkers))

(lang-c/add-hook 'lang-c/change-checker)

(setq-default sh-basic-offset tab-width)

(package-use 'd-mode :require t)

(setq self-inserting-characters '("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "\\" "z" "x" "c" "v" "b" "n" "m" "," "." "/" "TAB" "SPC" "<tab>" "<space>" "~" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "|" ">" "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "DEL"))

(defun make-normal-sparse-keymap()
  (setq result (make-sparse-keymap))
  (dolist (char self-inserting-characters)
    (define-key result (kbd char) 'ignore))
  result)

(setq leader-map (make-sparse-keymap))

(general-define-key
 :keymaps 'leader-map
  "SPC" 'execute-extended-command
  "s" 'save-some-buffers
  "b" 'ivy-switch-buffer
  "f" 'find-file
  "d" 'dired
  "k" 'kill-buffer
  "RET" 'eshell/toggle
  "<return>" 'eshell/toggle
  "C-RET" 'eshell/new
  "C-<return>" 'eshell/new
  "e b" 'eval-buffer
  "e r" 'eval-region
  "e e" 'eval-expression)

(general-define-key
 "C-z" nil
 "C-SPC" leader-map
 "C-@" leader-map
 "<escape>" (kbd "C-g")
 "M-q" (lambda() (interactive) (lmodal-raise-default-mode))
 "M-e" (lambda() (interactive) (lmodal-disable-all-modes)))

(setq modal/normal-bare-map (make-sparse-keymap))

(general-define-key
 :keymaps 'modal/normal-bare-map
 "k" 'previous-line
 "K" 'scroll-down-command
 "j" 'next-line
 "J" 'scroll-up-command
 "h" 'backward-char
 "H" 'backward-word
 "l" 'forward-char
 "L" 'forward-word
 "a" 'beginning-of-line
 "f" 'end-of-line
 "SPC" leader-map)

(setq modal/normal-map (make-composed-keymap (list (copy-keymap modal/normal-bare-map)) (make-normal-sparse-keymap)))

(general-define-key
 :keymaps 'modal/normal-map
 "q" (lambda() (interactive) (lmodal-raise-mode insert))
 "Q" 'edit/insert-beginning-of-line
 "r" 'edit/insert-after
 "R" 'edit/insert-end-of-line
 "e" 'edit/set-region
 "E" 'edit/set-region-line
 "s" 'edit/copy-whole-line
 "S" 'edit/copy-whole-line
 "d" (kbd "C-d")
 "D" 'kill-whole-line
 "w" 'yank
 "W" 'edit/yank-line
 "/" 'swiper
 "u" 'undo-tree-undo
 "U" 'undo-tree-redo
 "n" 'edit/open-line
 "N" (lambda() (interactive) (edit/open-line) (lmodal-raise-mode insert))
 "p" 'edit/open-line-above
 "P" (lambda() (interactive) (edit/open-line-above) (lmodal-raise-mode insert))
 "g" nil
 "g l" 'isearch-forward
 "g h" 'isearch-backward
 "g c" 'avy-goto-char
 "g l" 'avy-goto-line
 "m" 'edit/insert-mark
 "M" 'edit/goto-mark
 "TAB" 'origami-toggle-node
 "<tab>" 'origami-toggle-node)

(setq modal/region-map (make-composed-keymap (list (copy-keymap modal/normal-bare-map)) (make-normal-sparse-keymap)))

(general-define-key
 :keymaps 'modal/region-map
 "t" (lambda() (interactive) (kill-region (region-beginning) (region-end)) (lmodal-raise-mode insert))
 "s" (lambda() (interactive) (copy-region-as-kill (region-beginning) (region-end)) (lmodal-raise-default-mode))
 "d" (lambda() (interactive) (kill-region (region-beginning) (region-end)) (lmodal-raise-default-mode))
 "w" 'edit/yank-region
 "C-g" (lambda() (interactive) (pop-mark) (lmodal-raise-default-mode))
 "M-q" (lambda() (interactive) (pop-mark) (lmodal-raise-default-mode))
 "<escape>" (lambda() (interactive) (pop-mark) (lmodal-raise-default-mode))
 "e" 'er/expand-region
 "TAB" (lambda() (interactive) (indent-region (region-beginning) (region-end)) (lmodal-raise-default-mode))
 "<tab>" (lambda() (interactive) (indent-region (region-beginning) (region-end)) (lmodal-raise-deafult-mode))
 "g" nil
 "g l" 'isearch-forward
 "g h" 'isearch-backward
 ";" 'comment-or-uncomment-region
 "o" nil
 "o (" (lambda() (interactive) (edit/surround-region "(" ")") (lmodal-raise-default-mode))
 "o o" (lambda() (interactive) (edit/surround-region (read-from-minibuffer "left: ") (read-from-minibuffer "right: ")) (lmodal-raise-default-mode))
 "o )" (lambda() (interactive) (edit/surround-region "(" ")") (lmodal-raise-default-mode))
 "o {" (lambda() (interactive) (edit/surround-region "{" "}") (lmodal-raise-default-mode))
 "o }" (lambda() (interactive) (edit/surround-region "{" "}") (lmodal-raise-default-mode))
 "o [" (lambda() (interactive) (edit/surround-region "[" "]") (lmodal-raise-default-mode))
 "o ]" (lambda() (interactive) (edit/surround-region "[" "]") (lmodal-raise-default-mode))
 "o \"" (lambda() (interactive) (edit/surround-region "\"" "\"") (lmodal-raise-default-mode))
 "o <" (lambda() (interactive) (edit/surround-region "<" ">") (lmodal-raise-default-mode))
 "o '" (lambda() (interactive) (edit/surround-region "'" "'") (lmodal-raise-default-mode)))

(setq modal/insert-map (make-sparse-keymap))

(general-define-key
 :keymaps 'modal/insert-map
 "C-g" (lambda() (interactive) (lmodal-raise-default-mode)))

(general-define-key
 :keymaps 'company-active-map
 "<tab>" 'company-complete
 "TAB" 'company-complete)

(defun set-eshell-custom-map()
  (general-define-key
    :keymaps 'eshell-mode-map
    "C-SPC" leader-map
    "C-@" leader-map))

(add-hook 'eshell-mode-hook 'set-eshell-custom-map)

(require 'dired)
(setq dired-mode-map (make-composed-keymap (list (copy-keymap modal/normal-bare-map)) dired-mode-map))

(general-define-key
 :keymaps 'dired-mode-map
 "q" (lambda() (interactive) (wdired-change-to-wdired-mode) (lmodal-raise-default-mode)))

(setq modal/wdired-normal-map (copy-keymap modal/normal-map))

(general-define-key
 :keymaps 'modal/wdired-normal-map
 "SPC s" (lambda() (interactive) (wdired-finish-edit) (lmodal-raise-default-mode)))

(setq modal/org-normal-map (copy-keymap modal/normal-map))

(general-define-key
 :keymaps 'modal/org-normal-map
 "TAB" 'org-cycle
 "<tab>" 'org-cycle)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "M-j" 'ivy-next-line
 "M-k" 'ivy-previous-line
 "M-RET" 'ivy-immediate-done
 "TAB" 'ivy-partial-or-done
 "RET" 'ivy-done)

(lmodal-ignore-major-mode eshell-mode)
(lmodal-ignore-major-mode dired-mode)
(lmodal-ignore-major-mode ibuffer-mode)

(lmodal-define-mode normal :keymap modal/normal-map :lighter " [N]" :doc "Normal mode"
                    :on-enable (setq cursor-type config/default-cursor))
(lmodal-define-mode insert :keymap modal/insert-map :lighter " [I]" :doc "Insert mode"
                    :on-enable (setq cursor-type config/insert-cursor))
(lmodal-define-mode region :keymap modal/region-map :lighter " [R]" :doc "Region mode"
                    :on-enable (setq cursor-type config/default-cursor))
(lmodal-define-mode wdired-normal :keymap modal/wdired-normal-map :lighter " [N]" :doc "Normal mode for wdired"
                    :on-enable (setq cursor-type config/default-cursor))
(lmodal-define-mode org-normal :keymap modal/org-normal-map :lighter " [N]" :doc "Normal mode for org"
                    :on-enable (setq cursor-type config/default-cursor))

(lmodal-set-default-mode normal)
(lmodal-pair-major-mode wdired-mode wdired-normal)
(lmodal-pair-major-mode org-mode org-normal)

(lmodal-global-mode t)

(defun edit/surround(start end open close)
  (save-excursion
    (goto-char start)
    (insert open)
    (goto-char (+ end 1))
    (insert close)))

(defun edit/surround-region(open close)
  (when (region-active-p)
    (edit/surround (region-beginning) (region-end) open close)))

(defun edit/insert-after()
  (interactive)
  (forward-char)
  (lmodal-raise-mode insert))

(defun edit/insert-end-of-line()
  (interactive)
  (end-of-line)
  (lmodal-raise-mode insert))

(defun edit/insert-beginning-of-line()
  (interactive)
  (beginning-of-line)
  (lmodal-raise-mode insert))

(defun edit/set-region()
  (interactive)
  (set-mark (point))
  (lmodal-raise-mode region))

(defun edit/set-region-line()
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line)
  (lmodal-raise-mode region))

(defun edit/open-line()
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line))

(defun edit/open-line-above()
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun edit/yank-line()
  (interactive)
  (save-excursion
    (edit/open-line)
    (yank)
    (delete-blank-lines)))

(defun edit/kill-whole-word()
  (interactive)
  (backward-char)
  (forward-word)
  (backward-kill-word 1))

(defun edit/copy-whole-line()
  (interactive)
  (save-excursion
    (kill-whole-line)
    (yank)))

(defun edit/yank-region()
  (interactive)
  (kill-region (region-beginning) (region-end))
  (yank 2)
  (lmodal-raise-default-mode))

(defun edit/insert-mark()
  (interactive)
  (insert "<++>"))

(defun edit/goto-mark()
  (interactive)
  (search-forward "<++>")
  (search-backward "<")
  (delete-char  4)
  (lmodal-raise-mode insert))

(defun macro-make-function(&optional name)
  (interactive)
  (if (called-interactively-p 'any)
      (setq name (read-string "Macro name: "))
    (setq name (if name name "last-macro")))
  (setq function-string (pp-to-string (elmacro-make-defun (make-symbol (concat "macros/" name)) (elmacro-extract-last-macro elmacro-command-history))))
  (message function-string)
  (set-buffer (generate-new-buffer "*temporaryMacroBuffer*"))
  (erase-buffer)
  (insert function-string)
  (eval-buffer)
  (message function-string)
  (kill-buffer "*temporaryMacroBuffer*"))

(defun eshell/get-last-eshell-buffer()
  (catch 'buffer
    (dolist (buffer (buffer-list))
      (when (cl-search "*eshell*" (buffer-name buffer))
        (throw 'buffer buffer)))))

(defun eshell/switch-to-last-eshell-buffer()
  (let ((buffer (eshell/get-last-eshell-buffer)))
    (if buffer
        (switch-to-buffer buffer)
      (eshell))))

(defun eshell/toggle()
  (interactive)
  (if (cl-search "*eshell" (buffer-name))
      (switch-to-prev-buffer)
    (eshell/switch-to-last-eshell-buffer)))

(setq eshell/new-count 1)
(defun eshell/new()
  (interactive)
  (eshell eshell/new-count)
  (setq eshell/new-count (+ 1 eshell/new-count)))

(add-hook 'eshell-mode-hook (lambda() (interactive) (linum-mode 0)))

(package-use 'all-the-icons :require t)
(package-use 'all-the-icons-dired :require t)

(setq-default header-line-format '(" %b"))

(setq-default mode-line-format '(" %l : %c : %i"))

(defun theme/tty()
  (set-face-attribute 'company-tooltip nil
          :background "#FFFFFF"))

(defun theme/gui()
  (setq font-name (or
        (catch 'font-name
          (dolist (font '("Cascadia Mono" "Consolas"))
            (when (find-font (font-spec :name font))
              (throw 'font-name font)))) "monospace"))

  (set-face-attribute 'default nil
                      :family font-name
                      :height 112)
  (set-face-attribute 'linum nil
                      :height 'unspecified
                      :inherit 'default))

(defun theme/general()
  (set-face-attribute 'header-line nil
                      :foreground (face-attribute 'linum :foreground)
                      :background (face-attribute 'default :background)
                      :height (- (face-attribute 'default :height) 5)
                      :inherit nil)

  (set-face-attribute 'header-line-highlight nil
                      :foreground (face-attribute 'header-line :foreground)
                      :background (face-attribute 'header-line :background)
                      :height (face-attribute 'header-line :height)
                      :inherit nil)

  )

(defun theme/reload()
  (interactive)
  (if (and (display-graphic-p) (not (daemonp)))
      (theme/gui)
    (theme/tty))
  (theme/general))

(advice-add 'load-theme :after (lambda(&rest args) (theme/reload)))

(load-theme 'arc-dark)