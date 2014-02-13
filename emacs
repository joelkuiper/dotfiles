;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;; special thanks to
;; https://github.com/krisajenkins/EvilBegins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d" emacs-major-version))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))

(require 'package)
(package-initialize)

(defvar my-packages '(;; Core
                      evil evil-leader
                      auto-complete
                      ace-jump-mode
                      key-chord
                      exec-path-from-shell
                      ido-ubiquitous
                      ;; Themes
                      leuven-theme
                      ;; Project management
                      magit ;; git
                      projectile
                      sunrise-commander
                      ;; Writing
                      org org-plus-contrib htmlize
                      langtool ;; Spellcheck
                      ;; Language support
                      ess ;; R
                      ein python-mode ;; Python
                      cider cider-tracing clojure-test-mode ;; Clojure
                      web-mode js2-mode ;; Web development
                      highlight paredit evil-paredit pretty-mode-plus ;; LISP
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

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(require 'evil-leader)
(require 'ace-jump-mode)

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
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)

;; Leaders
(evil-leader/set-key
  "g"  'magit-status
  "s"  'sunrise
  "f"  'find-file
  "b"  'switch-to-buffer
  "e"  'eshell
  "ws" 'whitespace-mode
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'projectile-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-everywhere t
      ido-use-virtual-buffers t
      ido-ubiquitous-mode 1
      ido-enable-flex-matching t)
(ido-mode t)

;; OSX ls doesn't support --dired. use brews' GNU core-utils
(when (and (memq window-system '(mac ns)) (executable-find "gls"))
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(setq inhibit-startup-screen +1)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Zoom with C-=/- (nice for presentations)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(show-paren-mode 1)
(global-font-lock-mode t)
(load-theme 'leuven t)
(setq ring-bell-function 'ignore)

;; Smooth scrolling
;; From http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      mouse-wheel-progressive-speed nil
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :weight 'normal
                    :width 'normal)
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :weight 'normal)))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Emacs Speaks Statistics
(require 'ess-site)

;; Lisp
(defun enable-lisp-utils ()
  (auto-complete-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (require 'evil-paredit)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(dolist (mode '(emacs-lisp-mode-hook
                lisp-mode-hook
                cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook mode
            '(lambda ()
               (enable-lisp-utils))))


;; File-types
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))

(require 'pretty-mode-plus)
(global-pretty-mode 1)

;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(require 'auto-complete-config)
(ac-config-default)

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

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda ()
                   (visual-line-mode 1)
                   (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(global-set-key (kbd "<f8>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

(dolist (mode '(lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

;; org-mode
(require 'org)
(require 'org-publish)
(require 'ox-bibtex)
(setq org-src-fontify-natively t
      org-export-with-smart-quotes t
      org-export-async-debug t
      org-confirm-babel-evaluate nil ;; yeah don't do anything stupid
      org-export-babel-evaluate nil
      org-html-head-include-default-style nil)

(setq org-latex-pdf-process (list "make; latexmk -pdf -bibtex -g %f"))
(setq org-latex-listings 't)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "times"))
(add-to-list 'org-latex-packages-alist '("protrusion=true,expansion=true" "microtype"))
(add-to-list 'org-latex-packages-alist '("usenames,dvipsnames" "xcolor"))

;; for bibtex2html see: http://foswiki.org/Tasks.Item11919
(when (memq window-system '(mac ns))
  (setenv "TMPDIR" "."))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (clojure . t)
   (emacs-lisp . t)
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

(defun org-custom-link-asset-follow (path)
  (org-open-file-with-emacs
   (format "./assets/%s" path)))

(defun org-custom-link-asset-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"assets/%s\" alt=\"%s\"/>" path desc))))

(org-add-link-type "asset" 'org-custom-link-asset-follow 'org-custom-link-asset-export)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations (from M-x customze-*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
