;; required in every emacs dotfile for installing
;; ..third party packages like projectile and others.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

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
 '(custom-enabled-themes (quote (wombat)))
 '(org-agenda-files (quote ("~/Documents/todos.org")))
 '(package-selected-packages (quote (neotree fountain-mode projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ============================================================================= ;;

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable menu bar
(menu-bar-mode -1)

;; disable the scrollbar
(toggle-scroll-bar -1)

;; disable toolbar
(tool-bar-mode -1)

;; enable projectile mode
(projectile-global-mode)

;; enable caching mode
(setq projectile-enable-caching t)

;; indexing folders for fast performance
(setq projectile-indexing-method 'native)

;; enable auto pairing of parenthesis
(electric-pair-mode 1)

;; show matching parens
(show-paren-mode 1)

;; handy functions - all functions can go below
(defun my/insert-line-and-move-cursor-above ()
  "Insert an empty line above the current line."
  (interactive)
    (beginning-of-line)
    (open-line))

;; text editing helpers
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m")) ;shift return to move cursor to next line
(global-set-key (kbd "<C-S-return>") 'my/insert-line--and-move-cursor-above)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; font face
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))


