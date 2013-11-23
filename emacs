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


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)

(defvar my-packages '(evil
                      auto-complete
                      python-mode
                      magit
                      ess
                      org
                      ace-jump-mode
                      highlight paredit evil-paredit
                      key-chord
                      projectile grizzl
                      web-mode js2-mode
                      flycheck
                      yasnippet
                      cider))

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
              (package-install package)))
          missing)
    ;; Close the compilation log.
    (let ((compile-window (get-buffer-window "*Compile-Log*")))
      (if compile-window
          (delete-window compile-window)))))

(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-startup-screen +1)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(set-fringe-mode 0)

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
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                "backups"))))

;; Visual
(show-paren-mode 1)
(global-font-lock-mode t)
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(set-default-font "Inconsolata-12")

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/dotfiles/themes/replace-colorthemes"))
(load-theme 'dark-font-lock t)
(set-background-color "#FEFEFE")
(setq ring-bell-function 'ignore)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(require 'ace-jump-mode)

(evil-mode 1)
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)

;; ESC quits all the things
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on
                                         "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save on focus lost
(when
   (and (featurep 'x) window-system)
 (defvar on-blur--saved-window-id 0 "Last known focused window.")
 (defvar on-blur--timer nil "Timer refreshing known focused window.")
 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (let* ((active-window (x-window-property
                          "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
          (active-window-id (if (numberp active-window)
                                active-window
                              (string-to-number
                               (format "%x00%x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
          (emacs-window-id (string-to-number
                            (frame-parameter nil 'outer-window-id))))
     (when (and
            (= emacs-window-id on-blur--saved-window-id)
            (not (= active-window-id on-blur--saved-window-id)))
       (run-hooks 'on-blur-hook))
     (setq on-blur--saved-window-id active-window-id)
     (run-with-timer 1 nil 'on-blur--refresh)))
 (add-hook 'on-blur-hook #'(lambda () (save-some-buffers t)))
 (on-blur--refresh))

;; Emacs Speaks Statistics
(require 'ess-site)

;; File-type stuff
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))

;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
(require 'grizzl)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "M-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "M-b") 'projectile-switch-to-buffer)

(require 'yasnippet)
(yas-global-mode 1)

;; org-mode
(require 'org)
(setq org-src-fontify-natively t)
(setq default-major-mode 'org-mode)
(setq org-export-with-smart-quotes t)
(setq org-html-head-include-default-style nil)
(setq org-html-postamble "<hr>
  <span xmlns:dct=\"http://purl.org/dc/terms/\" xmlns:vcard=\"http://www.w3.org/2001/vcard-rdf/3.0#\">
    <a rel=\"license\"
       href=\"http://creativecommons.org/publicdomain/zero/1.0/\">
      <img src=\"http://i.creativecommons.org/p/zero/1.0/80x15.png\" style=\"border-style: none;\" alt=\"CC0\" /></a>
  </span>
  %d
  <a href=\"https://twitter.com/%a\"><i class=\"fa fa-twitter\"></i>%a</a>")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (clojure . t)
   (emacs-lisp . t)
   (python . t)
   (lisp . t)
   (js . t)
   (sh . t)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'auto-complete-config)
(ac-config-default)

;; Python
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Lisp
(defun enable-lisp-utils ()
  (auto-complete-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (require 'evil-paredit)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(dolist (mode '(emacs-lisp-mode-hook clojure-mode-hook))
  (add-hook mode
            '(lambda ()
               (enable-lisp-utils))))

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
