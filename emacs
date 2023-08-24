;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 29)
  (error "This setup requires Emacs v29, or higher. You have: v%d" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq byte-compile-warnings nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-display-tree ()
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

(defun my-reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))


(defun my-determine-font-size ()
  "Determine the font size based on the hostname."
  (let ((hostname (system-name)))
    (cond ((string-equal hostname "Aether") 160)
          ((string-equal hostname "Theseus") 140)
          (t 140)))) ; Default font size

(defun my-set-font (&rest faces)
  "Set font attributes for the given FACES."
  (let* ((font-size (my-determine-font-size))
         (font-family "PragmataPro Liga")
         (font-weight 'normal)
         (font-width 'normal))
    (dolist (face faces)
      (set-face-attribute face nil
                          :family font-family
                          :height font-size
                          :weight font-weight
                          :width font-width))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  ;; Always, always, prefer UTF-8, anything else is insanity
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (winner-mode 1)
  (setq vc-follow-symlinks nil)
  (setq create-lockfiles nil)
  (setq tramp-terminal-type "tramp")

  (setq-default indent-tabs-mode nil)
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))


  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  (put 'erase-buffer 'disabled nil)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Packages

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG"))

(use-package vundo :ensure t)
(use-package direnv :ensure t)
(use-package diminish :ensure t)
(use-package eldoc :diminish eldoc-mode)
(use-package autorevert :diminish auto-revert-mode)

;; Persist history over Emacs restarts.
(use-package savehist :ensure t :init (savehist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(use-package tao-theme
  :ensure t
  :init
  (load-theme 'tao-yang t)
  :config
  (my-set-font 'minibuffer-prompt)
  (custom-theme-set-faces
   'tao-yang
   `(default ((t (:background "#fafafa" :foreground "#241F31"))))))


(my-set-font 'default)
(defface fallback '((t :family "PragmataPro"
                       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init '(cider magit dired)))

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo
        evil-want-keybinding nil ; https://github.com/emacs-evil/evil-collection
        evil-shift-width 2)
  (evil-mode 1)
  :config
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

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

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
  (define-key evil-normal-state-map (kbd "C-x <up>") 'windmove-up))

(use-package evil-leader
  :ensure t
  :after evil
  :init
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  :config
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
    "br"     'my-reload-buffer             ; BufferReload
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
    "d"      'my-display-tree
    "ln"     'display-line-numbers-mode

    "p;"     'counsel-projectile
    "pp"     'counsel-projectile
    "pf"     'counsel-projectile-find-file
    "ps"     'counsel-projectile-switch-project
    "pl"     'project-watch-log-file
    "pd"     'projectile-dired
    "pg"     'counsel-projectile-git-grep
    "P"      'counsel-yank-pop

    "ch"     'cider-repl-history
    "cb"     'cider-repl-clear-buffer
    "o."     'org-time-stamp
    "oe"     'org-export

    "u"      'vundo
    "|"      'evil-window-vsplit
    "_"      'evil-window-split
    "<left>" 'windmove-left
    "<right>"'windmove-right
    "<up>"   'windmove-up
    "<down>" 'windmove-down))

(use-package expand-region
  :ensure t
  :bind (:map evil-visual-state-map
              ("," . 'er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit :ensure t)
(use-package direnv :ensure t)
(use-package prescient :ensure t)

(use-package counsel-projectile
  :ensure t
  :diminish counsel-projectile-mode
  :after (projectile)
  :init
  (counsel-projectile-mode +1)
  :config
  (defun project-watch-log-file ()
    "Open and watch a log file from the project."
    (interactive)
    (let* ((project-root (projectile-project-root))
           (log-files (directory-files-recursively project-root "\\.log$"))
           (logfile (completing-read "Select log file: " log-files nil t)))
      (unless (file-exists-p logfile)
        (error "Log file does not exist: %s" logfile))
      (find-file logfile)
      (let ((auto-revert-verbose nil)) ;; Suppress verbose revert messages
        (auto-revert-tail-mode t))
      (message "Watching log file: %s" logfile))))

;; Autocomplete
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode)
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2)

  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  :config
  (my-set-font 'company-tooltip-common 'company-tooltip)

  (set-face-attribute 'company-scrollbar-bg nil
                      :background "#DFDEDC") ; background color of the scrollbar
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "#2D2D2C") ; foreground color of the scrollbar
  )



(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-project-search-path '("~/Repositories" "~/Sync/projects"))
  (setq projectile-sort-order 'recently-active)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-files '(".DS_Store" ".gitmodules" "kit-modules")))



(use-package company-prescient
  :ensure t
  :after counsel-projectile
  :init
  (company-prescient-mode))


;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t))

(use-package vertico-prescient
  :ensure t
  :after vertico
  :init
  (vertico-prescient-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :ensure t
  :init
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

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . lsp)
  :init
  (setq
   ;;lsp-enable-completion-at-point nil
   lsp-lens-enable nil
   lsp-eldoc-enable-hover nil
   lsp-file-watch-threshold 10000
   lsp-signature-auto-activate nil
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-indentation nil
   lsp-ui-sideline-enable nil
   lsp-enable-semantic-highlighting nil
   lsp-enable-symbol-highlighting nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-sideline-show-code-actions nil))

;; Web stuff
(use-package web-mode
  :ensure t
  :hook (web-mode . custom-web-mode-hook)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.less\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :init
  (defun custom-web-mode-hook ()
    "Hooks for Web mode."
    (flycheck-mode)
    (toggle-truncate-lines t)))

;; Emacs Speaks Statistics
(use-package ess
  :ensure t
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :init
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
  :after (evil)
  :diminish evil-cleverparens-mode
  :commands evil-cleverparens-mode)

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)



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
  :init
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
(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup))

(use-package flycheck-languagetool
  :ensure t
  :after (flycheck)
  :init
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
  :init
  ;; Enable specified languages
  (org-babel-do-load-languages 'org-babel-load-languages my-org-babel-languages)

  ;; Always redisplay inline images after executing SRC block
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tao-theme company-prescient vertico-prescient vertico prescient web-mode vundo unicode-fonts magit lsp-mode ivy-rich highlight-parentheses flycheck-languagetool expand-region exec-path-from-shell evil-leader evil-collection evil-cleverparens ess direnv diminish counsel-projectile company cider almost-mono-themes aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
