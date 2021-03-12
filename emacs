;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 27)
  (error "This setup requires Emacs v27, or higher. You have: v%d" emacs-major-version))

(setq user-full-name "Joël Kuiper"
      user-mail-address "me@joelkuiper.eu")

(load-file "~/.emacs.secrets")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(save-place-mode 1)

(defvar my-packages
  '(;; Core
    evil
    evil-matchit
    evil-leader
    exec-path-from-shell
    smex
    expand-region
    flycheck
    company
    flx
    undo-fu
    ;; Themes
    minimal-theme
    almost-mono-themes
    ;; Project management
    magit ; git
    projectile
    project-root
    counsel-projectile
    ag
    swiper
    ;; Writing
    adoc-mode
    org-plus-contrib
    org-ref
    htmlize
    ;; Language support
    emmet-mode
    lsp-mode lsp-ui lsp-ivy
    vue-mode ;; webdev
    ess ; R
    elpy ;; Python
    conda
    cider ; Clojure
    flycheck-clj-kondo
    markdown-mode
    json-mode
    less-css-mode
    scss-mode
    web-mode
    js2-mode
    ;; Lisp
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
(setq auto-window-vscroll nil)
(setq jit-lock-mode t)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'smex)
(smex-initialize)

(defun reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-fu)
(setq evil-undo-system 'undo-fu)
(require 'evil)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

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

(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)

;; Leaders
(evil-leader/set-key
  "."      'er/expand-region
  ","      'er/contract-region
  "/"      'comment-or-uncomment-region
  "x"      'smex                        ; eXecute
  "e"      'eval-expression
  "f"      'find-file
  "q"      'quit-window
  ";"      'swiper
  "r"      'counsel-recentf
  "sh"     'ansi-term                   ; SHell
  "bs"     'counsel-switch-buffer
  "br"     'reload-buffer               ; BufferReload
  "bk"     'ido-kill-buffer
  "df"     'magit-diff-dwim
  "ws"     'whitespace-mode
  "gg"     'magit
  "gd"     'magit-diff-unstaged
  "gs"     'magit-status
  "gb"     'magit-blame
  "gc"     'magit-commit
  "gl"     'magit-log-all
  "gP"     'magit-push-current-to-upstream
  "gp"     'magit-pull-from-upstream

  "p;"     'counsel-projectile
  "pf"     'counsel-projectile-find-file
  "ps"     'counsel-projectile-switch-project
  "pd"     'projectile-dired
  "pg"     'counsel-projectile-ag
  "P"      'counsel-evil-registers

  "lr"     'lsp-find-references
  "ld"     'lsp-find-definition
  "lD"     'lsp-find-declaration

  "|"      'evil-window-vsplit
  "_"      'evil-window-split
  "<left>" 'windmove-left
  "<right>"'windmove-right
  "<up>"   'windmove-up
  "<down>" 'windmove-down)


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
(load-theme 'almost-mono-white t)

(set-background-color "#FAFAFA")
(set-foreground-color "#111111")

(setq font-lock-maximum-decoration nil)
(setq font-lock-maximum-size nil)

(fringe-mode '(0 . 0))
(set-face-attribute
 'default nil
 :family "PragmataPro"
 :height 120
 :weight 'normal
 :width 'normal)

(setq default-frame-alist
      (append (list '(internal-border-width . 12)
                    '(font . "PragmataPro 12"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 12)

(defface fallback '((t :family "PragmataPro 12"
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
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-initial-inputs-alist nil)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-project-search-path '("~/Repositories"))
(setq projectile-sort-order 'recentf)
(setq projectile-indexing-method 'hybrid)
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

(setq company-minimum-prefix-length 1
      company-idle-delay 0.1) ;; default is 0.2

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

;; See https://github.com/AdamNiederer/vue-mode/issues/74
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

(add-hook 'vue-mode-hook
          (lambda ()
            (subword-mode +1)
            (flyspell-mode -1)
            (electric-pair-mode +1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)

;; File-types
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
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

;; Lisp
(require 'evil-cleverparens)
(require 'flycheck-clj-kondo)

(defun enable-lisp-utils ()
  (aggressive-indent-mode)
  (paredit-mode)
  (evil-cleverparens-mode)
  (highlight-parentheses-mode t)
  (flycheck-mode))

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

(setq cider-auto-select-error-buffer nil)

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

(setq lsp-enable-completion-at-point nil
      ;;lsp-lens-enable t
      lsp-file-watch-threshold 10000
      lsp-signature-auto-activate nil
      lsp-headerline-breadcrumb-enable nil
      lsp-enable-indentation nil
      lsp-ui-sideline-enable nil
      lsp-enable-semantic-highlighting nil
      lsp-enable-symbol-highlighting nil
      lsp-modeline-code-actions-enable nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-show-code-actions nil)


;; Company
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;; Python
(defun enable-python-utils ()
  ;; pip install jedi rope flake8 autopep8 yapf black
  (pyvenv-activate "/home/joelkuiper/anaconda3/")
  (elpy-enable))
(add-hook 'python-mode-hook 'enable-python-utils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flycheck-define-checker
    proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode adoc-mode))

(add-to-list 'flycheck-checkers 'proselint)

(defun enable-write-utils ()
  (flyspell-mode t)
  (visual-line-mode 1))

(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(setq sentence-end-double-space nil)

;; org-mode
(require 'org)

(require 'ox-publish)
(require 'ox-latex)
(require 'ox-bibtex)
(setq org-src-fontify-natively t
      org-export-with-smart-quotes t
      org-fontify-whole-heading-line t
      org-startup-with-inline-images (display-graphic-p)
      org-html-head-include-default-style nil)

(defun org-custom-link-asset-follow (path)
  (org-open-file-with-emacs
   (format "./assets/%s" path)))

(defun org-custom-link-asset-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"/assets/%s\" alt=\"%s\"/>" path desc))))

(org-add-link-type "asset" 'org-custom-link-asset-follow 'org-custom-link-asset-export)

;; LaTeX-org-export
(setq org-latex-pdf-process (list "make; latexmk --gg --bibtex --pdf --latexoption=-shell-escape %f"))
(setq org-latex-listings 't)

(add-to-list 'org-latex-packages-alist '("" "times"))
(add-to-list 'org-latex-packages-alist '("protrusion=true,expansion=true" "microtype"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;;(R . t)
   (emacs-lisp . t)
   (python . t)
   (dot . t)
   (ditaa . t)
   (clojure . t)
   (plantuml . t)))

;; blogging
(setq org-publish-project-alist
      '(("org-joelkuiper"
         ;; Path to your org files.
         :base-directory "~/Remote/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/Remote/blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :with-toc nil
         :body-only t)

        ("org-static-joelkuiper"
         :base-directory "~/Remote/org/_posts/assets/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Remote/blog/assets/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("org-joelkuiper" "org-static-joelkuiper"))))

;; Tramp stuff
(setq tramp-terminal-type "tramp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations (from M-x customze-*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" default))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "dark orange")))))
