(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(defvar my-packages '(evil
                      auto-complete
                      paredit
                      ac-nrepl
                      molokai-theme
                      popup
                      pretty-symbols-mode
                      helm
                      helm-projectile
                      ido-ubiquitous
                      flx-ido
                      clojure-mode
                      nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Remove whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Visual
(setq molokai-theme-kit t)
(load-theme 'molokai t)
(global-font-lock-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-default-font "Source Code Pro")
(set-face-attribute 'default nil :font "Source Code Pro" :height 110)
(set-face-font 'default "Source Code Pro")
(line-number-mode 1)
(show-paren-mode 1)

;; Whitespace
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

;; Ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Projectile
(projectile-global-mode)
(global-set-key (kbd "C-c t") 'helm-projectile)

;; Evil
(require 'evil)
(evil-mode 1)
    (defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(require 'auto-complete-config)
(ac-config-default)

;; Clojure specific
(require 'nrepl)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(eval-after-load "auto-complete"
                 '(add-to-list 'ac-modes 'nrepl-mode))

;; Lisp
(defvar lisps (list 'scheme-mode-hook
                     'lisp-mode-hook
                     'emacs-lisp-mode-hook
                     'inferior-lisp-mode-hook
                     'inferior-scheme-mode-hook
                     'clojure-mode-hook))

(defun enable-lisp-utils ()
  (auto-complete-mode)
  (pretty-symbols-mode)
  (enable-paredit-mode))

(dolist (hook lisps)
  (add-hook hook 'enable-lisp-utils))

(custom-set-variables '(pretty-symbol-patterns
  (let ((lisps '(emacs-lisp-mode clojure-mode inferior-lisp-mode lisp-mode scheme-mode)))
    `(
      ;; Basic symbols, enabled by default
      (?λ lambda "\\<lambda\\>" (,@lisps))
      (?λ lambda "\\<fn\\>" (,@lisps))
      (?ƒ lambda "\\<defn\\>" (,@lisps))
      (?Ψ lambda "\\<psy\\>" (,@lisps))
      (?∂ lambda "\\<partial\\>" (,@lisps))
      (?ζ lambda "\\<apply\\>" (,@lisps))
      ;; Relational operators --
      (?≠ lambda "/=" (,@lisps))
      (?≥ lambda ">=" (,@lisps))
      (?≤ lambda "<=" (,@lisps))
      (?Ø lambda "nil" (,@lisps))))))
