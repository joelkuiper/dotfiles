(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(evil
                      auto-complete
                      paredit
                      ac-nrepl
                      molokai-theme
                      pretty-mode
                      ido-ubiquitous
                      clojure-mode
                      nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; Evil
(require 'evil)
(evil-mode 1)

(require 'auto-complete-config)
(ac-config-default)

;; Clojure specific
(require 'nrepl)
(defun customize-clojure-mode ()
  (local-set-key "M-{" 'paredit-wrap-curly)
  (local-set-key "M-}" 'paredit-close-curly-and-newline)
  (local-set-key "M-[" 'paredit-wrap-square)
  (local-set-key "M-]" 'paredit-close-square-and-newline))

(add-hook 'clojure-mode-hook 'customize-clojure-mode)

;; Lisp
(defun enable-lisp-utils ()
  (auto-complete-mode)
  (pretty-mode)
  (enable-paredit-mode))

(defvar lisps (list 'scheme-mode-hook
                     'lisp-mode-hook
                     'emacs-lisp-mode-hook
                     'inferior-lisp-mode-hook
                     'inferior-scheme-mode-hook
                     'clojure-mode-hook))

(dolist (hook lisps)
  (add-hook hook 'enable-lisp-utils))

