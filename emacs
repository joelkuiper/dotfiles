;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 29)
  (error "This setup requires Emacs v29, or higher. You have: v%d" emacs-major-version))

(load-file "~/.emacs.secrets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq byte-compile-warnings nil)

(setq
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(defvar my-packages
  '(;; Core
    use-package))

(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages missing-packages)
      (or (package-installed-p package)
          (push package missing-packages)))))

(let ((missing (my-missing-packages)))
  (when missing
    ;; Check for new packages (package versions)
    (package-refresh-contents)
    ;; Install the missing packages
    (mapc (lambda (package)
            (when (not (package-installed-p package))
              (package-install package))) missing)
    ;; Close the compilation log.
    (let ((compile-window (get-buffer-window "*Compile-Log*")))
      (if compile-window
          (delete-window compile-window)))))

;; Always, always, prefer UTF-8, anything else is insanity
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG"))

(defun display-tree ()
  "Run the Unix tree command on the project root and display the output."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (current-file (buffer-file-name))
         (tree-output
          (shell-command-to-string
           (format "tree %s -P '%s'" project-root
                   (file-relative-name current-file project-root)))))
    (with-output-to-temp-buffer "*tree*"
      (princ tree-output))))


(defun reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init)
  (evil-collection-init 'magit)
  (evil-collection-init 'dired)
  (evil-collection-init 'ivy))

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo
        evil-want-keybinding nil ; https://github.com/emacs-evil/evil-collection
        evil-shift-width 2)
  :config
  (evil-mode 1)
  ;; Shift left/right functions and bindings
  (defun shift-left-visual ()
    "Shift left and restore visual selection."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun shift-right-visual ()
    "Shift right and restore visual selection."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


  (define-key evil-visual-state-map (kbd ">") 'shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'shift-left-visual)
  (define-key evil-visual-state-map (kbd ".") 'er/expand-region)

  (define-key evil-normal-state-map (kbd "C-x <right>") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-x <left>") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-x <down>") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-x <up>") 'windmove-up)

  (define-key evil-insert-state-map (kbd "§") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "§") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "±") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "±") 'evil-normal-state))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  ;; Leaders
  (evil-leader/set-key
    "1"      (lambda () (interactive) (find-file "~/.emacs"))
    "2"      (lambda () (interactive) (find-file "~/Sync/org/scratch.org"))
    "3"      (lambda () (interactive) (find-file "~/Sync/org/todo.org"))
    "4"      (lambda () (interactive) (find-file "~/Sync/org/journal.org"))
    "5"      (lambda () (interactive) (find-file "~/Sync/org/repl.org"))

    "*nl"    (lambda () (interactive)
               (setq-local ispell-dictionary "nl"
                           flycheck-languagetool-language "nl-NL"))
    "*en"    (lambda () (interactive)
               (setq-local ispell-dictionary "en_US"
                           flycheck-languagetool-language "en-US"))

    "."      'er/expand-region
    ","      'er/contract-region
    "/"      'comment-or-uncomment-region
    "x"      'counsel-M-x               ; eXecute
    "e"      'eval-expression
    "f"      'counsel-fzf
    "q"      'quit-window
    ";"      'swiper
    "r"      'counsel-buffer-or-recentf
    "sh"     'eshell                    ; SHell
    "bs"     'counsel-ibuffer
    "br"     'reload-buffer             ; BufferReload
    "bk"     'ido-kill-buffer
    "ws"     'whitespace-mode
    "gg"     'magit
    "gd"     'magit-diff-unstaged
    "gs"     'magit-status
    "gb"     'magit-blame
    "gc"     'magit-commit
    "gl"     'magit-log-all
    "gP"     'magit-push-current-to-upstream
    "gp"     'magit-pull-from-upstream
    "d"      'display-tree
    "m"      'counsel-semantic-or-imenu
    "ln"     'display-line-numbers-mode

    "p;"     'counsel-projectile
    "pf"     'counsel-projectile
    "ps"     'counsel-projectile-switch-project
    "pd"     'projectile-dired
    "pg"     'counsel-projectile-ag
    "P"      'counsel-yank-pop

    "ch"     'cider-repl-history
    "cb"     'cider-repl-clear-buffer

    "lr"     'lsp-find-references
    "lD"     'lsp-find-declaration
    "ld"     'lsp-ui-doc-glance

    "u"      'vundo
    "|"      'evil-window-vsplit
    "_"      'evil-window-split
    "<left>" 'windmove-left
    "<right>"'windmove-right
    "<up>"   'windmove-up
    "<down>" 'windmove-down))

(use-package evil-cleverparens :ensure t)

(use-package expand-region
  :ensure t
  :bind (:map evil-visual-state-map
              ("," . 'er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-white t))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup)
  (defface fallback '((t :family "PragmataPro"
                         :inherit 'face-faded)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback))
  (set-display-table-slot standard-display-table 'selective-display
                          (string-to-vector " …")))

(put 'erase-buffer 'disabled nil)
(set-background-color "#fafafa")

(use-package diminish :ensure t)

(use-package eldoc :diminish eldoc-mode)
(use-package autorevert :diminish auto-revert-mode)


(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(winner-mode 1)
(setq vc-follow-symlinks nil)
(setq create-lockfiles nil)
(setq tramp-terminal-type "tramp")

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit :ensure t)

(use-package ivy-rich
  :ensure t
  :after ivy
  :diminish ivy-rich-mode)

(use-package counsel
  :ensure t
  :diminish counsel-mode)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :after counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (ivy-rich-mode 1)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (projectile-global-mode)
  (setq projectile-project-search-path '("~/Repositories" "~/Sync/projects"))
  (setq projectile-sort-order 'recently-active)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-files '(".DS_Store" ".gitmodules" "kit-modules")))

(use-package counsel-projectile
  :ensure t
  :diminish counsel-projectile-mode
  :after (ivy counsel projectile)
  :config
  (counsel-projectile-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(use-package whitespace
  :ensure t
  :config
  ;; whitespace configurations...
  (setq whitespace-line-column 80)
  (setq whitespace-style
        '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))

  (add-hook
   'before-save-hook
   (lambda ()
     (whitespace-cleanup)
     (delete-trailing-whitespace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

;; Web stuff
(use-package web-mode
  :ensure t
  :hook (web-mode . custom-web-mode-hook)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.less\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :config
  (defun custom-web-mode-hook ()
    "Hooks for Web mode."
    (flycheck-mode)
    (toggle-truncate-lines t)))

;; Emacs Speaks Statistics
(use-package ess
  :ensure t
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :config
  (setq ess-eval-visibly 'nowait)
  (setq ess-r-backend 'lsp))


;; Lisp
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode)

(use-package evil-cleverparens
  :ensure t
  :diminish evil-cleverparens-mode
  :commands evil-cleverparens-mode)

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . lsp)
  :config
  (setq
   company-minimum-prefix-length 1
   lsp-enable-completion-at-point nil
   lsp-lens-enable nil
   lsp-eldoc-enable-hover nil
   lsp-file-watch-threshold 10000
   lsp-signature-auto-activate nil
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-indentation nil
   lsp-ui-sideline-enable nil
   ;;lsp-enable-semantic-highlighting nil
   lsp-enable-symbol-highlighting nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   ;; lsp-ui-doc-show-with-cursor nil
   lsp-ui-sideline-show-code-actions nil))

(defun enable-lisp-utils ()
  (aggressive-indent-mode)
  (paredit-mode)
  (evil-cleverparens-mode)
  (highlight-parentheses-mode t))

(defvar lisps
  '(emacs-lisp-mode lisp-mode ielm-mode cider-mode cider-repl-mode
                    clojurescript-mode clojurec-mode clojurex-mode clojure-mode))

(dolist (mode lisps)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'enable-lisp-utils))

(use-package cider
  :ensure t
  :commands cider-jack-in
  :hook ((clojure-mode . cider-mode)
         (clojurescript-mode . cider-mode)
         (clojurec-mode . cider-mode))
  :config
  (setq cider-eldoc-display-symbol-at-point nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-repl-print-length 100)
  (setq cider-repl-print-level 10)
  (setq cider-repl-use-pretty-printing t))

;; Indentation and code style
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )

(defun my-code-style ()
  (interactive)
  (setq-default indent-tabs-mode nil
                default-tab-width 2
                evil-shift-width tab-width)
  (my-setup-indent 2))

(my-code-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging (org mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck-languagetool
  :ensure t
  :after (flycheck)
  :config
  (setq flycheck-languagetool-server-jar
        "~/Sync/etc/LanguageTool/languagetool-server.jar"))

(defun enable-write-utils ()
  (flycheck-languagetool-setup)
  (flycheck-mode)
  (visual-line-mode 1))

(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(setq sentence-end-double-space nil)

;; org-mode
(defconst my-org-babel-languages
  '((R . t)
    (dot . t)
    (clojure . t)
    (emacs-lisp . t)
    (shell . t)
    (sqlite . t))
  "Languages to enable in Org mode.")

(defun my-org-confirm-babel-evaluate (lang body)
  (not (assoc-string lang my-org-babel-languages t))) ; don't ask for languages in my-org-babel-languages

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(use-package org
  :ensure t
  :mode (("\\.org\\'" . org-mode))
  :config
  ;; Enable specified languages
  (org-babel-do-load-languages 'org-babel-load-languages my-org-babel-languages)

  ;; Always redisplay inline images after executing SRC block
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-languagetool web-mode unicode-fonts magit lsp-mode ivy-rich highlight-parentheses flycheck expand-region exec-path-from-shell evil-leader evil-collection evil-cleverparens ess diminish counsel-projectile company cider almost-mono-themes aggressive-indent)))
