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

(load-file "~/.emacs.secrets")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(package-initialize)

(defvar my-packages '(;; Core
                      evil evil-leader
                      key-chord
                      exec-path-from-shell
                      flx-ido
                      ido-ubiquitous
                      smex
                      ;; Themes
                      leuven-theme
                      pretty-mode
                      ;; Project management
                      magit ;; git
                      projectile
                      ;; Writing
                      org-plus-contrib htmlize
                      langtool ;; Spellcheck
                      ;; Language support
                      ess ;; R
                      cider ;; Clojure
                      web-mode js2-mode ;; Web development
                      highlight paredit evil-paredit rainbow-delimiters;; LISP
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
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSX Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (or (eq system-type 'darwin) (memq window-system '(mac ns)))
  (setq gc-cons-threshold (* 40 1024 1024))

  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  (setq mac-option-modifier nil
        mac-command-modifier 'meta)

  ;; for bibtex2html see: http://foswiki.org/Tasks.Item11919
  (setenv "TMPDIR" ".")

  ;; BSD ls doesn't support --dired. use brews' GNU core-utils
  (when (executable-find "gls")
    (setq
     dired-use-ls-dired t
     dired-listing-switches "-alh"
     insert-directory-program "gls")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(require 'smex)
(smex-initialize)

(defun reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; emacs striptease http://bzg.fr/emacs-strip-tease.html
;; A small minor mode to use a big fringe
(defvar big-fringe-mode nil)
(define-minor-mode big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable big-fringe-mode
  :group 'editing-basics
  (if (not big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(evil-set-initial-state 'magit-mode 'emacs)

(setq evil-shift-width 2)
(put 'narrow-to-region 'disabled nil) ;narrow to region should be enabled by default

;; Leaders
(evil-leader/set-key
  "x"  'smex ;; eXecute
  "e"  'eval-expression
  "g"  'magit-status
  "f"  'find-file
  "sh" 'eshell ; SHell
  "bs" 'switch-to-buffer ; BufferSwitch
  "br" 'reload-buffer ; BufferReload
  "bk" 'ido-kill-buffer
  "k"  'kill-this-buffer
  "u"  'undo-tree-visualize
  "ws" 'whitespace-mode
  "D"  'dired
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'projectile-grep)

(require 'undo-tree)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)
(global-undo-tree-mode 1)

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
      ido-max-prospects 10
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

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

(global-font-lock-mode t)
(load-theme 'leuven t)

;; From http://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
(add-hook 'text-mode-hook 'variable-pitch-mode)
(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(show-paren-mode t)

(require 'pretty-mode)
(pretty-activate-groups '(:greek :undefined))
(global-pretty-mode t)

;; No bell
(setq ring-bell-function 'ignore)

(when (window-system)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (mouse-wheel-mode t))

(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans Condensed"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(setq
   version-control t
   kept-new-versions 6
   kept-old-versions 2
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs.d/saves"))
   delete-old-versions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace
(set-default 'tab-width 2)
(set-default 'indicate-empty-lines t)
(set-default 'indent-tabs-mode nil)

;; Also highlight long lines in whitespace-mode
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style
      '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))

;; Cleanup whitespace on save
(add-hook 'before-save-hook
          (lambda ()
            (whitespace-cleanup)
            (delete-trailing-whitespace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File-types
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.js.?" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rd\\'" . Rd-mode))


;; Emacs Speaks Statistics
(require 'ess-site)
(setq ess-use-ido t)

;; Lisp
(defun enable-lisp-utils ()
  (require 'evil-paredit)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (rainbow-delimiters-mode)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-lisp-utils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I am dyslectic
(when (file-exists-p "/usr/local/Cellar/languagetool/2.3/libexec/languagetool-commandline.jar")
  (require 'langtool)
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.3/libexec/languagetool-commandline.jar"
        langtool-mother-tongue "nl"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")))

(when (executable-find "hunspell")
  (setq-default flyspell-issue-welcome-flag nil
                ispell-program-name "hunspell"
                ispell-really-hunspell t)
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

(defun enable-write-utils ()
  (visual-line-mode 1)
  (electric-indent-mode 1)
  (flyspell-mode 1))

(dolist (hook '(text-mode-hook org-mode-hook latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; org-mode
(require 'org)
(when (not (version= (org-version) "8.2.7c"))
  (display-warning
   'org-mode
   (concat
    "Insufficient requirements. Expected 8.2.7c. Found " (org-version))
   :warning))

(require 'ox-publish)
(require 'ox-latex)
(require 'ox-bibtex)
(setq org-src-fontify-natively t
      org-export-with-smart-quotes t
      org-fontify-whole-heading-line t
      org-confirm-babel-evaluate nil
      org-export-with-section-numbers nil
      org-export-babel-evaluate nil
      org-startup-with-inline-images (display-graphic-p)
      org-html-head-include-default-style nil)

(eval-after-load "org"
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block 'org-table 'org-block-background)))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "VERIFY" "|" "SUSPENDED" "DONE")))

(defun org-custom-link-asset-follow (path)
  (org-open-file-with-emacs
   (format "./assets/%s" path)))

(defun org-custom-link-asset-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"/assets/%s\" alt=\"%s\"/>" path desc))))

(org-add-link-type "asset" 'org-custom-link-asset-follow 'org-custom-link-asset-export)


(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/7994/plantuml.7994.jar"))

;; LaTeX-org-export
(setq org-latex-pdf-process (list "make; latexmk --bibtex --pdf --latexoption=-shell-escape %f"))
(setq org-latex-listings 't)

(add-to-list 'org-latex-packages-alist '("" "times"))
(add-to-list 'org-latex-packages-alist '("protrusion=true,expansion=true" "microtype"))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (clojure . t)
   (emacs-lisp . t)
   (plantuml . t)
   (dot . t)
   (python . t)
   (lisp . t)
   (ditaa . t)))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
