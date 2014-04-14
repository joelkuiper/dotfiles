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

(setq user-full-name "Joël Kuiper"
      user-mail-address "me@joelkuiper.eu")

(load-file "~/.emacs.secrets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(package-initialize)

(defvar my-packages '(;; Core
                      evil evil-leader
                      yasnippet
                      auto-complete
                      ace-jump-mode
                      key-chord
                      exec-path-from-shell
                      flx-ido
                      ido-ubiquitous
                      smex
                      browse-kill-ring
                      ;; web-browser
                      w3m
                      ;; Themes
                      leuven-theme monokai-theme
                      ;; Project management
                      magit ;; git
                      projectile
                      ;; Writing
                      org-plus-contrib htmlize
                      langtool ;; Spellcheck
                      ;; Language support
                      ess ;; R
                      elpy ;; python
                      cider ;; Clojure
                      web-mode js2-mode ;; Web development
                      highlight paredit evil-paredit pretty-mode ;; LISP
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

(setq inhibit-splash-screen t)
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

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
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)

;; Leaders
(evil-leader/set-key
  "x"  'smex
  "g"  'magit-status
  "f"  'find-file
  "b"  'switch-to-buffer
  "e"  'eshell
  "k"  'ido-kill-buffer
  "y"  'browse-kill-ring
  "u"  'undo-tree-visualize
  "ws" 'whitespace-mode
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'projectile-grep
  "id" 'duckduckgo-search
  "iw" 'wikipedia-search)

;; Always, always, prefer UTF-8, anything else is insanity
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-faces t)
(ido-mode 'buffer)

;; ido support pretty much everwhere
(require 'ido-ubiquitous)
(ido-ubiquitous)

;; BSD ls doesn't support --dired. use brews' GNU core-utils
(when (and (memq window-system '(mac ns)) (executable-find "gls"))
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(menu-bar-mode -1)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t))
(tool-bar-mode -1)
(setq make-pointer-invisible t)

;; http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
(winner-mode 1)

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

(require 'saveplace)
(setq-default save-place t)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      version-control t
      auto-save-default nil
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Visual
(show-paren-mode 1)
(global-font-lock-mode t)
(load-theme 'leuven t)

(require 'pretty-mode)
(global-pretty-mode t)

;; No bell
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12.4
                               :weight 'normal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; Whitespace
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq tab-width 2)

;; Also highlight long lines in whitespace-mode
(require 'whitespace)
(setq whitespace-line-column 79)
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

;; Emacs Speaks Statistics
(require 'ess-site)

;; Lisp
(defun enable-lisp-utils ()
  (require 'evil-paredit)
  (enable-paredit-mode)
  (evil-paredit-mode t))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-lisp-utils))

;; File-types
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org$\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.js.?" . js2-mode))

;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sentence-end-double-space nil)

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

(dolist (hook '(org-mode-hook latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; org-mode
(require 'org)
(require 'ox-publish)
(require 'ox-odt)
(require 'ox-latex)
(require 'ox-bibtex)
(setq org-src-fontify-natively t
      org-export-with-smart-quotes t
      org-confirm-babel-evaluate nil ;; yeah don't do anything stupid
      org-export-with-section-numbers nil
      org-html-include-timestamps nil
      org-export-babel-evaluate nil
      org-html-head-include-default-style nil)

;; for bibtex2html see: http://foswiki.org/Tasks.Item11919
(when (memq window-system '(mac ns))
  (setenv "TMPDIR" "."))

(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/7994/plantuml.7994.jar"))

;; LaTeX-org-export
(setq org-latex-pdf-process (list "make; latexmk -gg -bibtex -pdf -latexoption=-shell-escape -f -silent %f"))
(setq org-latex-listings 't)

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "times"))
(add-to-list 'org-latex-packages-alist '("" "epstopdf"))
(add-to-list 'org-latex-packages-alist '("" "makeidx"))
(add-to-list 'org-latex-packages-alist '("protrusion=true,expansion=true" "microtype"))
(add-to-list 'org-latex-packages-alist '("usenames,dvipsnames" "xcolor"))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

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
;;; Browsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web browsing http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
;; change default browser for 'browse-url' to w3m
(when (executable-find "w3m")
  (setq w3m-command (executable-find "w3m"))
  (require 'w3m)

  (setq
   browse-url-browser-function 'w3m-browse-url
   w3m-confirm-leaving-secure-page nil
   w3m-use-cookies t
   ;; Change w3m user-agent to Android.
   w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.0.1; en-us; Droid Build/ESD56) AppleWebKit/530.17 (KHTML, like Gecko) Version/4.0 Mobile Safari/530.17"
   ;; Set homepage
   w3m-home-page "duckduckgo.com/lite")

  (defun wikipedia-search (search-term)
    "Search for SEARCH-TERM on wikipedia"
    (interactive
     (let ((term (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (word-at-point))))
       (list
        (read-string
         (format "Wikipedia (%s):" term) nil nil term))))
    (browse-url
     (concat
      "http://en.m.wikipedia.org/w/index.php?search=" search-term)))

  (defun duckduckgo-search (search-term)
    "Search for SEARCH-TERM"
    (interactive
     (let ((term (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (word-at-point))))
       (list
        (read-string
         (format "DuckDuckGo (%s):" term) nil nil term))))
    (browse-url
     (concat "https://duckduckgo.com/lite/?q=" search-term))))

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
