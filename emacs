(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(evil
                      auto-complete
                      paredit
                      molokai-theme
                      fuzzy-match
                      slime
                      clojure-mode
                      nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Theme
(setq molokai-theme-kit t)
(load-theme 'molokai t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "Mensch-10")

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)

;; Evil
(require 'evil)
(evil-mode 1)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Clojure specific
(require 'nrepl)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(defun customize-clojure-mode ()
  (paredit-mode 1)
  (local-set-key "M-{" 'paredit-wrap-curly)
  (local-set-key "M-}" 'paredit-close-curly-and-newline)
  (local-set-key "M-[" 'paredit-wrap-square)
  (local-set-key "M-]" 'paredit-close-square-and-newline))

(add-hook 'clojure-mode-hook 'customize-clojure-mode)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defface esk-paren-face
  '((((class color) (background dark))
      (:foreground "grey50"))
    (((class color) (background light))
      (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                            '(("(\\|)" . 'esk-paren-face))))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192"
                                                           'decompose-region)))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)