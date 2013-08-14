(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(evil
                      auto-complete
                      anything
                      paredit
                      ac-nrepl
                      molokai-theme
                      popup
                      pretty-symbols-mode
                      ido-ubiquitous
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

;; Theme
(setq molokai-theme-kit t)
(load-theme 'molokai t)
(global-font-lock-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(ido-mode t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

;; Anything
(global-set-key (kbd "C-x t") 'anything-mini)

;; Evil
(require 'evil)
(evil-mode 1)

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

(dolist (hook lisps)
  (add-hook hook 'enable-lisp-utils))

(custom-set-variables '(pretty-symbol-patterns
  (let ((lisps '(emacs-lisp-mode clojure-mode inferior-lisp-mode lisp-mode scheme-mode)))
    `(
      ;; Basic symbols, enabled by default
      (?λ lambda "\\<lambda\\>" (,@lisps))
      (?λ lambda "\\<fn\\>" (,@lisps))
      (?ƒ lambda "\\<defn\\>" (,@lisps))
      (?ƒ lambda "\\<defn-\\>" (,@lisps))
      (?∂ lambda "\\<partial\\>" (,@lisps))
      (?ζ lambda "\\<apply\\>" (,@lisps))
      ;; Relational operators --
      (?≠ lambda "/=" (,@lisps))
      (?≥ lambda ">=" (,@lisps))
      (?≤ lambda "<=" (,@lisps))
      (?Ø lambda "nil" (,@lisps))))))

(defun enable-lisp-utils ()
  (auto-complete-mode)
  (pretty-symbols-mode)
  (enable-paredit-mode))
