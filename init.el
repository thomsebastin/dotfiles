;; required in every emacs dotfile for installing
;; ..third party packages like projectile and others.
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; to make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; use inconsolata font
(set-default-font "Inconsolata 12")

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable menu bar
(menu-bar-mode -1)

;; disable the scrollbar
(toggle-scroll-bar -1)

;; disable toolbar
(tool-bar-mode -1)

;; enable auto pairing of parenthesis
(electric-pair-mode 1)

;; show matching parens
(show-paren-mode 1)

;; overwrite selected text
(delete-selection-mode t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; converting multiple cursors to use 'use-package'
(use-package multiple-cursors
  :ensure t
  :commands multiple-cursors
  :init
  :bind (
            ("C-c m c" . mc/edit-lines)
            ("C-d"     . mc/mark-next-like-this)
            ("C-D"     . mc/mark-previous-like-this)
            ("C-c C-<" . mc/mark-all-like-this)
))

;; enable projectile mode
(use-package projectile
  :ensure  projectile
  :config
    (projectile-global-mode t)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'native))

;; neotree
(use-package neotree
  :bind ([f8] . neotree-toggle))

;; fountain mode
(use-package fountain-mode)

;; typescript mode
;; (use-package typescript-mode)

;; enable company-mode
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-global-modes '(not org-mode)))

(add-hook 'prog-mode-hook 'global-company-mode)
(define-key company-mode-map (kbd "TAB") 'company-complete)

;; enable line number in prog-mode only
(add-hook 'prog-mode-hook 'linum-mode)

;; text editing helpers
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m")) ;shift return to move cursor to next line
(global-set-key (kbd "<C-S-return>") 'my/insert-line-and-move-cursor-above)

;; handy functions - all functions can go below
(defun my/insert-line-and-move-cursor-above()
  "Insert an empty line above the current line."
  (interactive)
    (beginning-of-line)
    (open-line 1))

;; functionality for moving a line/region up/down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; TSX settings
;;(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;(add-hook 'web-mode-hook
;;          (lambda ()
;;            (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;              (setup-tide-mode))))
;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(setq tide-format-options '(
  :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
  :placeOpenBraceOnNewLineForFunctions nil
  :indentSize: 2,
  :tabSize: 2,
  :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces: t))


(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)


;; ============================================================================= ;;

;; this is added by emacs when you change the theme
;; ..using M-x customize-themes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/Documents/todos.org")))
 '(package-selected-packages
   (quote
    (web-mode company multiple-cursors typescript-mode neotree fountain-mode projectile)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ============================================================================= ;;
