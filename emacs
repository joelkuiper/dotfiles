;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration of emacs + evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 29)
  (error "This setup requires Emacs v29, or higher. You have: v%d" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))


(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-display-tree ()
  "Run the Unix tree command on the project root and display the output."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (current-file (buffer-file-name))
         (tree-output
          (shell-command-to-string
           (format "tree %s -P '%s'" project-root
                   (file-relative-name current-file project-root)))))
    (with-output-to-temp-buffer "*tree*"
      (princ tree-output))))

(defun my-reload-buffer ()
  "revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun my-determine-font-size ()
  "Determine the font size based on the hostname."
  (let ((hostname (system-name)))
    (cond ((string-equal hostname "Aether") 160)
          ((string-equal hostname "Theseus") 140)
          (t 140)))) ; Default font size

(defun my-set-font (&rest faces)
  "Set font attributes for the given FACES."
  (let* ((font-size (my-determine-font-size))
         (font-family "PragmataPro Liga")
         (font-weight 'normal)
         (font-width 'normal))
    (dolist (face faces)
      (set-face-attribute face nil
                          :family font-family
                          :height font-size
                          :weight font-weight
                          :width font-width))))

(defun my-project-watch-log-file ()
  "Open and watch a log file from the project."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (log-files (directory-files-recursively project-root "\\.log$"))
         (logfile (completing-read "Select log file: " log-files nil t)))
    (unless (file-exists-p logfile)
      (error "Log file does not exist: %s" logfile))
    (find-file logfile)
    (let ((auto-revert-verbose nil)) ;; Suppress verbose revert messages
      (auto-revert-tail-mode t))
    (message "Watching log file: %s" logfile)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  ;; Always, always, prefer UTF-8, anything else is insanity
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)

  (winner-mode 1)
  (setq vc-follow-symlinks nil)
  (setq create-lockfiles nil)
  (setq tramp-terminal-type "tramp")

  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setq completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setq completions-detailed t)                        ; Show annotations
  (setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

  (put 'erase-buffer 'disabled nil)

  (setq scroll-step 1)
  (setq scroll-margin 3))


;; Packages
(use-package vundo :ensure t)
(use-package direnv :ensure t)
(use-package diminish :ensure t)
(use-package eldoc :diminish eldoc-mode)
(use-package autorevert :diminish auto-revert-mode)
(use-package vterm :ensure t)

;; Persist history over Emacs restarts.
(use-package savehist :ensure t :init (savehist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

;; Highlights hex codes
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (text-mode prog-mode))

(use-package ligature
  :ensure t
  :config
  ;; Enable ligatures everywhere
  (ligature-set-ligatures 't '("---" "--" "ff" "fi" "fl" "ffi" "ffl"))
  ;; Enable all ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("[INFO ]" "[WARN ]" "[PASS ]" "[VERBOSE]" "[KO]" "[OK]" "[PASS]"
     "[ERROR]" "[DEBUG]" "[INFO]" "[WARN]" "[WARNING]" "[ERR]"
     "[FATAL]" "[TRACE]" "[FIXME]" "[TODO]" "[BUG]" "[NOTE]" "[HACK]"
     "[MARK]" "[FAIL]" "!=" "!==" "!≡≡" "!=<" "#(" "#_" "#{" "#?" "##"
     "#_(" "#[" "%=" "&%" "&&" "&+" "&-" "&/" "&=" "&&&" "$>" "(|"
     "*>" "++" "+++" "+=" "+>" "++=" "--" "-<" "-<<" "-=" "->" "->>"
     "-->" "-+-" "-\\/" "-|>" "-<|" "->-" "-<-" "-|" "-||" "-|:"
     ".=" "//=" "/=" "/==" "/-\\" "/-:" "/->" "/=>" "/-<" "/=<" "/=:"
     ":=" ":=" ":=>" ":-\\" ":=\\" ":-/" ":=/" ":-|" ":=|" ":|-" ":|="
     "<$>" "<*" "<*>" "<+>" "<-" "<<=" "<=" "<=>" "<>" "<|>" "<<-"
     "<|" "<=<" "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>" "<%>"
     "<^>" "<&>" "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>"
     "<<^" "<=" "<->" "<!--" "<--" "<~<" "<==>" "<|-" "<||" "<<|"
     "<-<" "<-->" "<<==" "<==" "<-\\" "<-/" "<=\\" "<=/" "=<<" "=="
     "===" "==>" "=>" "=~" "=>>" "=~=" "==>>" "=>=" "=<=" "=<" "==<"
     "=<|" "=/" "=/=" "=/<" "=|" "=||" "=|:" ">-" ">=" ">>-" ">>="
     ">=>" ">>^" ">>|" ">!=" ">->" ">==" ">=" ">/=" ">-|" ">=|" ">-\\"
     ">=\\" ">-/" ">=/" "?." "^=" "^^" "^<<" "^>>" "\\=" "\\=="
     "\\/-" "\\-/" "\\-:" "\\->" "\\=>" "\\-<" "\\=<" "\\=:" "|="
     "|>=" "|>" "|+|" "|->" "|-->" "|=>" "|==>" "|>-" "|<<" "||>"
     "|>>" "|-" "||-" "||=" "|)" "|]" "|-:" "|=:" "|-<" "|=<" "|--<"
     "|==<" "~=" "~>" "~~>" "~>>" "[[" "[|" "_|_" "]]"))
  (global-ligature-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo
        evil-want-keybinding nil ; https://github.com/emacs-evil/evil-collection
        evil-shift-width 2)
  (evil-mode 1)
  :config
  ;; Shift left/right functions and bindings
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

  (setq evil-respect-visual-line-mode t)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)

  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


  (define-key evil-visual-state-map (kbd ">") 'shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'shift-left-visual)
  (define-key evil-visual-state-map (kbd ".") 'er/expand-region)

  (define-key evil-normal-state-map (kbd "C-x <right>") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-x <left>") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-x <down>") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-x <up>") 'windmove-up))

(use-package evil-collection
  :ensure t
  :requires (evil)
  :diminish evil-collection-unimpaired-mode
  :after (cider corfu magit dired vterm vundo org)
  :init
  (evil-collection-init
   '(cider corfu magit dired vterm vundo org)))

(use-package evil-leader
  :ensure t
  :after evil
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  ;; Leaders
  (evil-leader/set-key
    "1"      (lambda () (interactive) (find-file "~/.emacs"))
    "2"      (lambda () (interactive) (find-file "~/Sync/org/scratch.org"))
    "3"      (lambda () (interactive) (find-file "~/Sync/org/todo.org"))
    "4"      (lambda () (interactive) (find-file "~/Sync/org/journal.org"))
    "5"      (lambda () (interactive) (find-file "~/Sync/org/repl.org"))

    "/"      'comment-or-uncomment-region
    "."      'er/expand-region
    ","      'er/contract-region
    "x"      'counsel-M-x               ; eXecute
    "e"      'eval-expression
    "f"      'counsel-fzf
    "q"      'quit-window
    ";"      'swiper
    "r"      'counsel-buffer-or-recentf
    "sh"     'eshell                    ; SHell
    "te"     'vterm                     ; TErm
    "vt"     'vterm                     ; VTerm
    "bs"     'counsel-ibuffer
    "br"     'my-reload-buffer          ; BufferReload
    "bk"     'ido-kill-buffer
    "ws"     'whitespace-mode
    "d"      'my-display-tree
    "ln"     'display-line-numbers-mode

    "gg"     'magit
    "gd"     'magit-diff-unstaged
    "gs"     'magit-status
    "gb"     'magit-blame
    "gc"     'magit-commit
    "gl"     'magit-log-all
    "gP"     'magit-push-current-to-upstream
    "gp"     'magit-pull-from-upstream

    "p;"     'counsel-projectile
    "pp"     'counsel-projectile
    "pf"     'counsel-projectile-find-file
    "ps"     'counsel-projectile-switch-project
    "pl"     'project-watch-log-file
    "pd"     'projectile-dired
    "pg"     'counsel-projectile-git-grep
    "P"      'counsel-yank-pop

    "ch"     'cider-repl-history
    "cb"     'cider-repl-clear-buffer
    "o."     'org-time-stamp
    "oe"     'org-export

    "u"      'vundo
    "|"      'evil-window-vsplit
    "_"      'evil-window-split
    "<left>" 'windmove-left
    "<right>"'windmove-right
    "<up>"   'windmove-up
    "<down>" 'windmove-down))

(use-package expand-region
  :ensure t
  :bind (:map evil-visual-state-map
              ("," . 'er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit :ensure t)
(use-package direnv :ensure t)

(use-package counsel-projectile
  :ensure t
  :diminish counsel-projectile-mode
  :after (projectile)
  :init
  (counsel-projectile-mode +1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  :config
  (setq projectile-project-search-path
        '("~/Repositories" "~/Sync/Repositories" "~/Projects"))
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-globally-ignored-files
        '(".DS_Store" ".gitmodules" "kit-modules")))

;; Autocomplete
(require 'ffap)

(defun my-ffap-completion-at-point ()
  (let ((fn (ffap-guesser)))
    (when (and fn (not (string-match " " fn)))
      (let* ((dir (file-name-directory fn))
             (file (file-name-nondirectory fn))
             (files (and dir (file-name-all-completions file dir))))
        (list (length file)
              (point)
              (mapcar (lambda (f) (concat dir f)) files))))))

(add-to-list 'completion-at-point-functions 'my-ffap-completion-at-point)

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator))

(use-package prescient
  :ensure t
  :config
  (setq completion-styles '(prescient)))

(use-package corfu-prescient
  :ensure t
  :requires (corfu prescient)
  :init
  (corfu-prescient-mode))

(use-package vertico-prescient
  :ensure t
  :requires (vertico prescient)
  :init
  (vertico-prescient-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :ensure t
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style
        '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))

  (add-hook
   'before-save-hook
   (lambda ()
     (whitespace-cleanup)
     (delete-trailing-whitespace))))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck :ensure t :diminish flycheck-mode)
(use-package flymake :ensure t :diminish flymake-mode)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((clojure-mode clojurescript-mode clojurec-mode R-mode) . lsp)
  :config
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(prescient)
                completion-category-defaults nil))
  (add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)
  (setq
   ;;lsp-enable-completion-at-point nil
   lsp-lens-enable nil
   lsp-eldoc-enable-hover nil
   lsp-file-watch-threshold 10000
   lsp-signature-auto-activate nil
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-indentation nil
   lsp-ui-sideline-enable nil
   ;;lsp-enable-semantic-highlighting nil
   ;;lsp-enable-symbol-highlighting nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-sideline-show-code-actions nil))

;; Web stuff
(use-package web-mode
  :ensure t
  :defer t
  :hook (web-mode . custom-web-mode-hook)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :init
  (defun custom-web-mode-hook ()
    "Hooks for Web mode."
    (toggle-truncate-lines t)))

;; Emacs Speaks Statistics (R)
(use-package ess
  :ensure t
  :defer t
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :init
  (setq ess-eval-visibly 'nowait)
  (setq ess-r-backend 'lsp))

;; Lisp
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode)

(use-package evil-cleverparens
  :ensure t
  :after (evil)
  :diminish evil-cleverparens-mode
  :commands evil-cleverparens-mode)

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)

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

(dolist (mode lisps)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'enable-lisp-utils))

(use-package cider
  :ensure t
  :commands cider-jack-in
  :hook ((clojure-mode . cider-mode)
         (clojurescript-mode . cider-mode)
         (clojurec-mode . cider-mode))
  :config
  (setq cider-eldoc-display-symbol-at-point nil
        cider-auto-select-error-buffer nil
        cider-repl-print-length 100
        cider-repl-use-pretty-printing t))

;; Indentation and code style
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )

(defun my-code-style ()
  (interactive)
  (setq-default
   indent-tabs-mode nil
   default-tab-width 2
   evil-shift-width tab-width)
  (my-setup-indent 2))

(my-code-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging (org mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-write-utils ()
 (flyspell-mode)
 (visual-line-mode 1))

(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook 'enable-write-utils))

(setq org-directory "~/Sync/org/")

(use-package org
  :ensure t
  :defer t
  :mode (("\\.org\\'" . org-mode))
  :config
  ;; org-mode
  (defconst my-org-babel-languages
    '((R . t)
      (dot . t)
      (clojure . t)
      (emacs-lisp . t)
      (shell . t)
      (sqlite . t))
    "Languages to enable in Org mode.")

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (assoc-string lang my-org-babel-languages t))) ; don't ask for languages in my-org-babel-languages

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; Enable specified languages
  (org-babel-do-load-languages 'org-babel-load-languages my-org-babel-languages)

  ;; Always redisplay inline images after executing SRC block
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tao-theme
  :ensure t
  :config
  (my-set-font 'default 'fixed-pitch)
  (custom-theme-set-faces
   'tao-yang

   ;; vterm colors customizations with :inherit background
   `(term-color-white ((t (:foreground "#616161"))))
   `(term-color-black ((t (:foreground "#241f31"))))
   `(term-color-red ((t (:foreground "#9C3328"))))
   `(term-color-green ((t (:foreground "DarkOliveGreen"))))
   `(term-color-yellow ((t (:foreground "DarkGoldenrod"))))
   `(term-color-blue ((t (:foreground "SkyBlue4"))))
   `(term-color-magenta ((t (:foreground "DarkMagenta"))))
   `(term-color-cyan ((t (:foreground "light blue")))))
  :init
  (load-theme 'tao-yang t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tao-theme cider highlight-parentheses evil-cleverparens paredit aggressive-indent ess web-mode orderless corfu marginalia vertico counsel-projectile magit expand-region evil-leader evil-collection ligature rainbow-mode vterm diminish direnv vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-key-binding ((t (:inherit default :background "grey96" :foreground "DarkBlue" :box (:line-width (-1 . -1) :color "grey80") :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#FCFCFC" :background "#616161" :inherit default)))))
