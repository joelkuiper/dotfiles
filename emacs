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
                      evil-matchit
                      exec-path-from-shell
                      flx-ido
                      ido-ubiquitous
                      smex
                      expand-region
                      flycheck
                      company
                      pretty-mode
                      avy
                      ;; Themes
                      theme-changer
                      leuven-theme ; light
                      material-theme ; dark
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
                      dash-at-point
                      polymode
                      coffee-mode
                      ess ; R
                      cider ; Clojure
                      clojure-mode-extra-font-locking
                      json-mode
                      company-tern
                      web-mode js2-mode ; Web development
                      highlight paredit evil-paredit rainbow-delimiters aggressive-indent ; Lisp
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
  (unless (eq tool-bar-mode -1)
    (tool-bar-mode -1))
  (unless (eq scroll-bar-mode -1)
    (scroll-bar-mode -1))
  ;; tooltips in echo-area
  (unless (eq tooltip-mode -1)
    (tooltip-mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSX Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (or (eq system-type 'darwin) (memq window-system '(mac ns)))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  (setq mac-option-modifier 'hyper
        mac-command-modifier 'meta)

  ;; for bibtex2html see: http://foswiki.org/Tasks.Item11919
  (setenv "TMPDIR" ".")

  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)

  (setq save-interprogram-paste-before-kill t)
  (setq x-select-enable-clipboard t)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; BSD ls doesn't support --dired. use brews' GNU core-utils
  (when (executable-find "gls")
    (setq
     dired-listing-switches "-alh"
     insert-directory-program "gls")))

(setq gc-cons-threshold (* 256 1024 1024)) ;; 256 mb
;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time 1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time)
(setq display-time-day-and-date t)

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
(require 'evil-matchit)
(global-evil-matchit-mode 1)

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

(define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
(define-key evil-motion-state-map (kbd "P") #'avy-goto-line)

(define-key evil-visual-state-map (kbd ".") 'er/expand-region)
; (define-key evil-visual-state-map (kbd ",") 'er/contract-region)

;; move between windows like a civilized fucking human being
(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-<right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-<left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-<down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-<up>") 'windmove-up)

;; Leaders
(evil-leader/set-key
  "SPC"    'avy-goto-subword-1
  "."      'er/expand-region
  ","      'er/contract-region
  "/"      'comment-or-uncomment-region
  "x"      'smex ;; eXecute
  "e"      'eval-expression
  "f"      'find-file
  "q"      'kill-this-buffer
  "sh"     'term; SHell
  "bs"     'switch-to-buffer ; BufferSwitch
  "br"     'reload-buffer ; BufferReload
  "bk"     'ido-kill-buffer
  "k"      'delete-window
  "u"      'undo-tree-visualize
  "d"      'vc-diff
  "m"      'dash-at-point ; Manual
  "ws"     'whitespace-mode
  "gs"     'magit-status
  "gb"     'magit-blame
  "gc"     'magit-commit
  "gl"     'magit-log-all
  "pt"     'projectile-find-tag
  "pf"     'projectile-find-file
  "ps"     'projectile-switch-project
  "pd"     'projectile-dired
  "pg"     'projectile-ag)

;; modes to map to different default states
(dolist (mode-map '((ag-mode . emacs)
                    (cider-repl-mode . emacs)
                    (eshell-mode . emacs)
                    (git-commit-mode . insert)
                    (git-rebase-mode . emacs)
                    (help-mode . emacs)
                    (term-mode . emacs)))
  (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
(add-hook 'with-editor-mode-hook 'evil-insert-state)

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

;; magic to change the mode-line color according to state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#af00d7" . "#ffffff"))
                                 ((evil-visual-state-p)  '("SteelBlue4" . "#ffffff"))
                                 ((evil-operator-state-p)  '("DarkSeaGreen4" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

(global-font-lock-mode t)


(require 'theme-changer)
(change-theme 'leuven 'material)

(show-paren-mode t)

(require 'pretty-mode)
(pretty-activate-groups '(:greek :undefined :arrow-tails :parentheses :types))
(global-pretty-mode t)

(set-fringe-mode '(1 . 1))

;; No bell
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '(font . "PragmataPro 12"))

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

(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-require-match nil
      company-frontends '(company-pseudo-tooltip-frontend)
      company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)

(setq
 ;; don't complete in certain modes
 company-global-modes '(not git-commit-mode)
 ;; make sure evil uses the right completion functions
 evil-complete-next-func 'company-complete-lambda
 evil-complete-previous-func 'company-complete-lambda)

;; Whitespace
(setq-default indent-tabs-mode  nil
              default-tab-width 2)

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

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
(setq flycheck-standard-error-navigation nil)

(global-flycheck-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; File-types
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.js.?" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))


;; Web stuff
(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (toggle-truncate-lines t))

(add-hook 'web-mode-hook 'custom-web-mode-hook)

(when (when (executable-find "tern"))
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook 'tern-mode))

;; Emacs Speaks Statistics
(require 'ess-site)
(setq ess-use-ido t)

;; Poly-R
(require 'poly-R)
(require 'poly-markdown)

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; Lisp
(defun enable-lisp-utils ()
  (require 'evil-paredit)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode)
  (evil-matchit-mode -1)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-lisp-utils))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; CoffeeScript
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map (kbd "<return>") 'coffee-newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-write-utils ()
  (flyspell-mode t)
  (visual-line-mode 1))

(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(setq sentence-end-double-space nil)

(when (file-exists-p "/usr/local/Cellar/languagetool/2.8/libexec/languagetool-commandline.jar")
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.8/libexec/languagetool-commandline.jar"
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

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    (clojure . t)
;;    (emacs-lisp . t)
;;    (plantuml . t)
;;    (ditaa . t)
;;    (dot . t)
;;    (python . t)))

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
 '(display-time-mode t)
 '(fci-rule-color "#37474f")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(hl-sexp-background-color "#1c1f26")
 '(indicate-buffer-boundaries (quote left))
 '(package-selected-packages
   (quote
    (adoc-mode clojure-mode-extra-font-locking web-mode theme-changer smex rainbow-delimiters projectile pretty-mode polymode org-plus-contrib material-theme markdown-mode magit leuven-theme langtool key-chord json-mode js2-mode ido-ubiquitous htmlize highlight flycheck flx-ido expand-region exec-path-from-shell evil-paredit evil-matchit evil-leader ess dash-at-point company-tern coffee-mode cider avy aggressive-indent ag)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
