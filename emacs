;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d" emacs-major-version))

(setq user-full-name "Joël Kuiper"
      user-mail-address "me@joelkuiper.eu")

(setq calendar-location-name "Groningen, Groningen, NL")
(setq calendar-latitude 53.2166667)
(setq calendar-longitude 6.55)

(load-file "~/.emacs.secrets")
(load-library "url-handlers")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(require 'cl)
(package-initialize)

(defvar my-packages '(;; Core
                      evil
                      evil-leader
                      exec-path-from-shell
                      smex
                      expand-region
                      ;;ace-jump-mode
                      flycheck
                      company
                      ;; Themes
                      tao-theme
                      ;; Project management
                      magit ; git
                      projectile
                      counsel-projectile
                      ag
                      swiper
                      ;; Writing
                      adoc-mode
                      langtool
                      org-plus-contrib
                      org-ref
                      htmlize
                      ;; Language support
                      ess ; R
                      cider ; Clojure
                      markdown-mode
                      json-mode
                      less-css-mode
                      web-mode js2-mode
                      rainbow-delimiters highlight paredit evil-paredit aggressive-indent ; Lisp
                      ))

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

(tool-bar-mode -1)

(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode '(1 . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSX Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (or (eq system-type 'darwin) (memq window-system '(mac ns)))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  (setq mac-option-modifier 'none
        mac-command-modifier 'meta)

  (mac-auto-operator-composition-mode)

  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  ;; for bibtex2html see: http://foswiki.org/Tasks.Item11919
  (setenv "TMPDIR" ".")

  (setq-default fringes-outside-margins t)

  ;; BSD ls doesn't support --dired. use brews' GNU core-utils
  (when (executable-find "gls")
    (setq
     dired-listing-switches "-alh"
     insert-directory-program "gls")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smex)
(smex-initialize)

(defun reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
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
  "SPC"    'evil-ace-jump-char-mode
  "S-SPC"  'evil-ace-jump-word-mode
  "C-SPC"  'evil-ace-jump-line-mode
  "."      'er/expand-region
  ","      'er/contract-region
  "/"      'comment-or-uncomment-region
  "x"      'smex ; eXecute
  "e"      'eval-expression
  "f"      'find-file
  "q"      'quit-window
  "sh"     'term ; SHell
  "bs"     'switch-to-buffer ; BufferSwitch
  "br"     'reload-buffer ; BufferReload
  "bk"     'ido-kill-buffer
  "u"      'undo-tree-visualize
  "df"     'magit-diff-dwim
  "ws"     'whitespace-mode
  "gd"     'magit-diff-working-tree
  "gs"     'magit-status
  "gb"     'magit-blame
  "gc"     'magit-commit
  "gl"     'magit-log-all
  "p;"     'counsel-projectile
  "pf"     'counsel-projectile-find-file
  "ps"     'counsel-projectile-switch-project
  "pd"     'projectile-dired
  "pg"     'counsel-projectile-ag

  "|"      'evil-window-vsplit
  "_"      'evil-window-split
  "<left>" 'windmove-left
  "<right>"'windmove-right
  "<up>"   'windmove-up
  "<down>" 'windmove-down)

(define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)

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

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)
(load-theme 'tao-yang t)
(set-default-font "PragmataPro Mono 12")

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
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; Projectile
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
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
                default-tab-width 2)
  (my-setup-indent 2))

(my-code-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; File-types
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.js.?" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))


;; Web stuff
(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (toggle-truncate-lines t))

(add-hook 'web-mode-hook 'custom-web-mode-hook)

(when (executable-find "tern")
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook 'tern-mode))

;; Emacs Speaks Statistics
(require 'ess-site)

(defun add-pretty (binding)
  (push binding prettify-symbols-alist))

(defun prettify ()
  (add-pretty '("true"        .  ?т))
  (add-pretty '("false"       .  ?ғ))
  (add-pretty '(":keys"       .  ?ӄ))
  (add-pretty '(":strs"       .  ?ş))
  (add-pretty '("fn"          .  ?λ))
  (add-pretty '("lambda"      .  ?λ))
  (add-pretty '("nil"         .  ?Ø))
  (add-pretty '("partial"     .  ?∂))
  (add-pretty '("with-redefs" .  ?я))
  (add-pretty '("comp"        .  ?º))
  (add-pretty '("apply"       .  ?ζ))
  (add-pretty '("map"         .  ?↦))
  (add-pretty '("a-fn1"       .  ?α))
  (add-pretty '("a-fn2"       .  ?β))
  (add-pretty '("a-fn3"       .  ?γ))
  (add-pretty '("no-op"       .  ?ε)))

;; Lisp
(defun enable-lisp-utils ()
  (require 'evil-paredit)
  (aggressive-indent-mode)
  (show-paren-mode t)
  ;;(prettify)
  ;;(prettify-symbols-mode t)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (evil-paredit-mode t))

(defvar lisps '(emacs-lisp-mode
                lisp-mode
                ielm-mode
                cider-mode
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
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

(when (file-exists-p "/usr/local/Cellar/languagetool/3.4/libexec/languagetool-commandline.jar")
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.4/libexec/languagetool-commandline.jar"
        langtool-default-language "en-US"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")))

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
   (plantuml . t)
   ))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations (from M-x customze-*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
