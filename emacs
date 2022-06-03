
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 28)
  (error "This setup requires Emacs v28, or higher. You have: v%d" emacs-major-version))

(load-file "~/.emacs.secrets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(setq evil-undo-system 'undo-redo)
(setq evil-want-keybinding nil) ;; https://github.com/emacs-evil/evil-collection

(defvar my-packages
  '(;; Core
    evil
    evil-collection
    evil-leader
    evil-string-inflection
    exec-path-from-shell
    expand-region
    key-chord
    ssh-agency
    flycheck
    company
    flx
    ivy
    ivy-rich
    vundo
    direnv
    ;; Themes
    minimal-theme
    almost-mono-themes
    ;; Project management
    magit ; git
    projectile
    counsel-projectile
    ag
    swiper
    ;; Writing
    htmlize
    ;; Language support
    ess ; R
    markdown-mode
    lsp-mode lsp-ui lsp-ivy
    flycheck
    ;; Lisp
    cider ; Clojure
    highlight paredit evil-cleverparens
    highlight-parentheses
    aggressive-indent))

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
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "LC_ALL")
(exec-path-from-shell-copy-env "LANG")

(defun reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(evil-collection-init 'magit)
(evil-collection-init 'dired)
(evil-collection-init 'ivy)

(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)
(setq evil-shift-width 2)

(require 'expand-region)
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

(define-key evil-visual-state-map (kbd ">") 'shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'shift-left-visual)

(define-key evil-visual-state-map (kbd ".") 'er/expand-region)
;; (define-key evil-visual-state-map (kbd ",") 'er/contract-region)

(define-key evil-normal-state-map (kbd "C-x <right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-x <left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-x <down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-x <up>") 'windmove-up)

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


;; Leaders
(evil-leader/set-key
  "1"      (lambda () (interactive) (find-file "~/.emacs"))
  "2"      (lambda () (interactive) (find-file "~/org/ideas.org"))
  "3"      (lambda () (interactive) (find-file "~/org/todo.org"))

  "'"      'er/expand-region
  "."      'avy-goto-word-0
  ","      'er/contract-region
  "/"      'comment-or-uncomment-region
  "x"      'counsel-M-x                 ; eXecute
  "e"      'eval-expression
  "f"      'counsel-fzf
  "q"      'quit-window
  ";"      'swiper
  "r"      'counsel-buffer-or-recentf
  "sh"     'eshell                      ; SHell
  "bs"     'counsel-ibuffer
  "br"     'reload-buffer               ; BufferReload
  "bk"     'ido-kill-buffer
  "df"     'magit-diff-dwim
  "www"    'eww
  "news"   (lambda () (interactive) (eww-browse-url "https://lite.cnn.com"))
  "wiki"   (lambda () (interactive) (eww-browse-url "https://en.wikipedia.org/"))
  "ws"     'whitespace-mode
  "gg"     'magit
  "gd"     'magit-diff-unstaged
  "gs"     'magit-status
  "gb"     'magit-blame
  "gc"     'magit-commit
  "gl"     'magit-log-all
  "gP"     'magit-push-current-to-upstream
  "gp"     'magit-pull-from-upstream

  "m"      'counsel-semantic-or-imenu
  "ln"     'display-line-numbers-mode

  "p;"     'counsel-projectile
  "pf"     'counsel-projectile-find-file
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
  "<down>" 'windmove-down
  )

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
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'almost-mono-black t)

(setq font-lock-maximum-decoration nil)
(global-prettify-symbols-mode +1)
(fringe-mode '(0 . 0))

(setq default-frame-alist
      (append (list '(internal-border-width . 12))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 12)

(defface fallback '((t :family "PragmataPro"
                       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))



;; No bell
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(ivy-rich-mode 1)
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (counsel-projectile-switch-project . ivy--regex-plus)
        (counsel-ag . ivy--regex-plus)
        (counsel-rg . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

;; Projectile
(projectile-global-mode)
(setq projectile-project-search-path '("~/Repositories"))
(setq projectile-sort-order 'recently-active)
(setq projectile-indexing-method 'hybrid)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-files '(".DS_Store" ".gitmodules"))
(counsel-projectile-mode +1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(require 'lsp-mode)
(global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


;; Also highlight long lines in whitespace-mode
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style
      '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))

;; Cleanup whitespace on save (co-workers love this ;-))
(add-hook 'before-save-hook
          (lambda ()
            (whitespace-cleanup)
            (delete-trailing-whitespace)))

;; Indentation
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
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)

;; File-types
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js.?" . js2-mode))

;; Web stuff
(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (toggle-truncate-lines t))

(add-hook 'web-mode-hook 'custom-web-mode-hook)

;; Emacs Speaks Statistics
(require 'ess-site)
(setq ess-eval-visibly 'nowait)
(setq ess-r-backend 'lsp)


;; Lisp
(require 'evil-cleverparens)

(defun enable-lisp-utils ()
  (aggressive-indent-mode)
  (paredit-mode)
  (evil-cleverparens-mode)
  (highlight-parentheses-mode t))

(defvar lisps
  '(emacs-lisp-mode
    lisp-mode
    ielm-mode
    cider-mode
    cider-repl-mode
    clojurescript-mode
    clojurec-mode
    clojurex-mode
    clojure-mode))

(defun use-colon-as-word-start ()
  (let ((table (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?: "w" table)
    (set-syntax-table table)))
(add-hook 'clojure-mode-hook 'use-colon-as-word-start)

(dolist (mode lisps)
  ;; Add hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'enable-lisp-utils))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)


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
 lsp-enable-semantic-highlighting nil
 lsp-enable-symbol-highlighting nil
 lsp-modeline-code-actions-enable nil
 lsp-modeline-diagnostics-enable nil
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-sideline-show-code-actions nil)

(require 'cider)
(setq cider-eldoc-display-symbol-at-point nil)
(setq cider-auto-select-error-buffer nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(setq vc-follow-symlinks nil)
(defun enable-write-utils ()
  (visual-line-mode 1))

(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(setq sentence-end-double-space nil)

;; org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (clojure . t)
   (plantuml . t)))

;; Always redisplay inline images after executing SRC block
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

;; Tramp stuff
(setq tramp-terminal-type "tramp")

(put 'erase-buffer 'disabled nil)

(define-key evil-insert-state-map (kbd "§") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "§") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "±") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "±") 'evil-normal-state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations (from M-x customze-*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" default))
 '(package-selected-packages
   '(web-mode vundo undo-fu terraform-mode ssh-agency scss-mode pyenv-mode ox-gfm org-ref ob-ipython notmuch minimal-theme magit lsp-ui lsp-ivy key-chord js2-mode ivy-rich ivy-avy highlight-parentheses highlight flycheck flx expand-region exec-path-from-shell evil-string-inflection evil-leader evil-collection evil-cleverparens ess elpy direnv counsel-projectile conda cider almost-mono-themes aggressive-indent ag adoc-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
