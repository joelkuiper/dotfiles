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
                      flx-ido
                      ido-ubiquitous
                      smex
                      expand-region
                      ace-jump-mode
                      flycheck
                      company
                      ;;pretty-mode
                      ;; Themes
                      theme-changer
                      leuven-theme ; light
                      material-theme ; dark
                      tao-theme
                      ;; Project management
                      magit ; git
                      projectile
                      ag
                      ;; Writing
                      adoc-mode
                      langtool
                      org-plus-contrib
                      htmlize
                      ;; Language support
                      coffee-mode
                      ess ; R
                      cider ; Clojure
                      markdown-mode
                      json-mode
                      less-css-mode
                      company-tern
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

(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode '(1 . 1))
  (unless (eq tool-bar-mode -1)
    (tool-bar-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSX Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (or (eq system-type 'darwin) (memq window-system '(mac ns)))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  (setq mac-option-modifier 'none
        mac-command-modifier 'meta)

  ;;(mac-auto-operator-composition-mode)

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
  "q"      'kill-this-buffer
  "sh"     'term ; SHell
  "bs"     'switch-to-buffer ; BufferSwitch
  "br"     'reload-buffer ; BufferReload
  "bk"     'ido-kill-buffer
  "k"      'delete-window
  "u"      'undo-tree-visualize
  "d"      'magit-diff
  "ws"     'whitespace-mode
  "gs"     'magit-status
  "gb"     'magit-blame
  "gc"     'magit-commit
  "gl"     'magit-log-all
  "pt"     'projectile-find-tag
  "pf"     'projectile-find-file
  "pF"     'projectile-find-file-other-window
  "ps"     'projectile-switch-project
  "pd"     'projectile-dired
  "pg"     'projectile-ag

  "<left>" 'windmove-left
  "<right>"'windmove-right
  "<up>"   'windmove-up
  "<down>" 'windmove-down)

;; modes to map to different default states
(dolist (mode-map '((ag-mode . emacs)
                    (eshell-mode . emacs)
                    (git-commit-mode . insert)
                    (git-rebase-mode . emacs)
                    (help-mode . emacs)
                    (term-mode . emacs)))
  (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
(add-hook 'with-editor-mode-hook 'evil-insert-state)

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

(global-font-lock-mode t)

(require 'theme-changer)
(change-theme 'tao-yang 'tao-yin)

(show-paren-mode t)

;; No bell
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(add-to-list 'ido-ignore-files "\\.DS_Store")
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
(require 'projectile)
(projectile-global-mode)

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

(defun company-complete-lambda (arg)
  "Ignores passed in arg like a lambda and runs company-complete"
  (company-complete))

(setq
 ;; don't complete in certain modes
 company-global-modes '(not git-commit-mode)
 ;; make sure evil uses the right completion functions
 evil-complete-next-func 'company-complete-lambda
 evil-complete-previous-func 'company-complete-lambda)

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
  (setq coffee-tab-width n) ; coffeescript
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

(add-hook 'prog-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

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
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
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
(setq ess-use-ido t)

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
  (turn-on-eldoc-mode)
  (aggressive-indent-mode)
  (show-paren-mode t)
  (prettify)
  (prettify-symbols-mode t)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (evil-paredit-mode t))


(defvar lisps '(emacs-lisp-mode
                lisp-mode
                ielm-mode
                clojurescript-mode
                clojurec-mode
                clojurex-mode
                clojure-mode))

(dolist (mode lisps)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'enable-lisp-utils))

;; CoffeeScript
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map (kbd "<return>") 'coffee-newline-and-indent)))

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

(when (file-exists-p "/usr/local/Cellar/languagetool/3.2/libexec/languagetool-commandline.jar")
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.2/libexec/languagetool-commandline.jar"
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
      org-export-with-section-numbers nil
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#C2C2C2" "#161616" "#252525" "#080808" "#0E0E0E" "#161616" "#080808" "#080808"])
 '(custom-safe-themes
   (quote
    ("990efcb44fdf633d5e3752248a983629ed18712c05833709eddedfc076b9fa53" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" default)))
 '(display-time-mode t)
 '(fci-rule-color "#161616")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (web-mode theme-changer tao-theme sparql-mode smex rainbow-delimiters projectile polymode org-plus-contrib material-theme markdown-mode magit leuven-theme less-css-mode langtool json-mode js2-mode ido-ubiquitous htmlize highlight flycheck flx-ido expand-region exec-path-from-shell evil-paredit evil-leader ess company-tern coffee-mode cider aggressive-indent ag adoc-mode)))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9D9D9D")
     (60 . "#9D9D9D")
     (80 . "#C2C2C2")
     (100 . "#C2C2C2")
     (120 . "#D9D9D9")
     (140 . "#D9D9D9")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F0F0F0")
     (240 . "#F0F0F0")
     (260 . "#F0F0F0")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#F9F9F9")
     (360 . "#F9F9F9"))))
 '(vc-annotate-very-old-color "#D9D9D9"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :width normal :foundry "nil" :family "PragmataPro")))))
