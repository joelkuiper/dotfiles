;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                      ess
                      org remember
                      ace-jump-mode
                      rainbow-delimiters highlight paredit evil-paredit
                      key-chord
                      projectile
                      web-mode js2-mode
                      soft-charcoal-theme
                      flycheck
                      yasnippet
                      popup
                      slime
                      ido-ubiquitous flx-ido
                      clojure-mode
                      nrepl nrepl-eval-sexp-fu ac-nrepl))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; see http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Don't know why this is nessecary
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))

;; Visual
(global-linum-mode t)
(load-theme 'soft-charcoal t)
(global-font-lock-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(set-default-font "Inconsolata-12")

(require 'ace-jump-mode)

;; Evil
(require 'evil)
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

;; Whitespace
(setq tab-width 2)
(setq c-basic-offset 2)
(setq js-indent-level 2)
(setq sgml-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq evil-shift-width 2)

;; Remove whitespace on save
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

;; JS2-IDE
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-require-project-root nil)

(require 'yasnippet)
(yas-global-mode 1)

(require 'org)
(require 'org-remember)
(require 'remember)
(setq default-major-mode 'org-mode)
(add-hook 'remember-mode-hook 'org-mode)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (clojure . t)
   (emacs-lisp . t)
   (lisp . t)
   (js . t)
   (sh . t)))
(global-set-key (kbd "<f12>") 'remember)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'auto-complete-config)
(ac-config-default)

;; Lisp
(defun enable-lisp-utils ()
  (auto-complete-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (rainbow-delimiters-mode)
  (require 'evil-paredit)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (enable-lisp-utils)))

;;; Clojure
(add-hook 'clojure-mode-hook
          '(lambda ()
             (enable-lisp-utils)

             (mapc '(lambda (char)
                      (modify-syntax-entry char "w" clojure-mode-syntax-table))
                   '(?- ?_ ?/ ?< ?> ?: ?' ?.))

             (require 'ac-nrepl)
             (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
             (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
             (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
             (add-to-list 'ac-modes 'nrepl-mode)))

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
