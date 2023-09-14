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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (set-face-attribute
       face nil
       :family font-family
       :height font-size
       :weight font-weight
       :width font-width))))

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

  (setq-default
   ;; Mostly from https://github.com/angrybacon/dotemacs/blob/master/lisp/use-defaults.el
   inhibit-startup-screen t                         ;; No startup screen
   inhibit-startup-echo-area-message t              ;; No echo message
   initial-scratch-message nil                      ;; What's in the *scratch* buffer
   inhibit-startup-message t                        ;; No startup message
   initial-major-mode 'fundamental-mode             ;; What Emacs opens with
   enable-recursive-minibuffers t                   ;; Use the minibuffer whilst in the minibuffer
   completion-cycle-threshold 1                     ;; TAB cycles candidates
   completions-detailed t                           ;; Show annotations
   tab-always-indent 'complete                      ;; When I hit TAB, try to complete, otherwise, indent
   indent-tabs-mode nil                             ;; Prefer spaces over tabs to indent
   tramp-terminal-type "tramp"                      ;; Tramp compatibility
   ad-redefinition-action 'accept                   ;; Silence warnings for redefinitions
   auto-save-list-file-prefix nil                   ;; Prevent tracking for auto-saves
   backup-by-copying t                              ;; Backups never overwrite original
   backup-directory-alist `(("." . "~/.backups"))     ;; Where are the backups ?
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   comment-multi-line t                             ;; Continue comments when filling
   create-lockfiles nil                             ;; Locks are more nuisance than blessing
   cursor-in-non-selected-windows nil               ;; Hide the cursor in inactive windows
   cursor-type '(hbar . 2)                          ;; Underline-shaped cursor
   custom-file null-device                          ;; Prevent littering
   custom-unlispify-menu-entries nil                ;; Prefer kebab-case for titles
   custom-unlispify-tag-names nil                   ;; Prefer kebab-case for symbols
   delete-by-moving-to-trash t                      ;; Delete files to trash
   fill-column 80                                   ;; Set width for automatic line breaks
   gc-cons-threshold (* 8 1024 1024)                ;; We're not using Game Boys anymore
   help-window-select t                             ;; Focus new help windows when opened
   isearch-allow-scroll t                           ;; Allow scroll commands while isearching
   max-mini-window-height 10                        ;; Limit height for minibuffer transients
   mouse-yank-at-point t                            ;; Yank at point rather than pointer
   native-comp-async-report-warnings-errors 'silent ;; Skip error buffers
   byte-compile-warnings '(not obsolete)            ;; Disable obsolete warnings when compiling
   warning-suppress-log-types '((comp) (bytecomp))  ;; Suppress compiler warning messages
   read-process-output-max (* 1024 1024)            ;; Increase read size for data chunks
   recenter-positions '(5 bottom)                   ;; Set re-centering positions
   ring-bell-function 'ignore                       ;; Silence error bells
   ;; scroll-conservatively 101                        ;; Avoid recentering when scrolling far
   scroll-margin 3                                  ;; Add a margin when scrolling vertically
   scroll-step 1                                    ;; Add scroll step (like Vim)
   select-enable-clipboard t                        ;; Merge system's and Emacs' clipboard
   sentence-end-double-space nil                    ;; Use a single space after dots
   show-help-function nil                           ;; Disable help text everywhere
   tab-always-indent 'complete                      ;; Indent first then try completions
   uniquify-buffer-name-style 'forward              ;; Uniquify buffer names
   use-short-answers t                              ;; Replace yes/no prompts with y/n
   vc-follow-symlinks t                             ;; Never prompt when visiting symlinks
   version-control t                                ;; Use numeric versions for backups
   delete-old-versions t                            ;; Delete extra backups silently
   window-combination-resize t                      ;; Resize windows proportionally
   x-stretch-cursor t)                              ;; Stretch cursor to the glyph width

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (put 'erase-buffer 'disabled nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility packages and setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize and ensure that shell environment variables are set correctly.
;; Ensures that the Emacs shell environment matches the system's.
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; Provides undo/redo functionality.
;; Enables undo and redo commands in Emacs.
(use-package vundo
  :ensure t)

;; Integrates with direnv to load environment variables from .envrc.
(use-package direnv
  :ensure t)

;; Hides minor modes from the mode line.
(use-package diminish
  :ensure t)

;; Integrates Git version control into Emacs.
(use-package magit
  :ensure t)

;; Hides eldoc minor mode from the mode line.
(use-package eldoc
  :diminish eldoc-mode)

;; Hides auto-revert minor mode from the mode line.
(use-package autorevert
  :diminish auto-revert-mode)

;; Hides flymake mode from the mode line.
(use-package flymake
  :diminish flymake-mode)

;; Hides flycheck mode from the mode line.
(use-package flycheck
  :diminish flycheck-mode)

;; Saves and restores command history over Emacs restarts.
(use-package savehist
  :ensure t
  :init (savehist-mode))

;; Manages session persistence for Emacs.
(use-package desktop
  :ensure nil
  :defer 1
  :config
  (desktop-read)
  (desktop-save-mode))

;; Best thing ever
(use-package winner
  :config
  (winner-mode 1)
  (global-set-key (kbd "C-s-<left>") 'winner-undo)
  (global-set-key (kbd "C-s-<right>") 'winner-redo))

(global-set-key (kbd "C-M-s-<right>") 'windmove-right)
(global-set-key (kbd "C-M-s-<left>") 'windmove-left)
(global-set-key (kbd "C-M-s-<down>") 'windmove-down)
(global-set-key (kbd "C-M-s-<up>") 'windmove-up)

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
  :hook ((prog-mode . rainbow-mode)
         (text-mode . rainbow-mode)))

(use-package ligature
  :ensure t
  :config
  ;; Enable ligatures everywhere
  (ligature-set-ligatures
   't
   '("---" "--" "ff" "fi" "fl" "ffi" "ffl"
     "[INFO ]" "[WARN ]" "[PASS ]" "[VERBOSE]" "[KO]" "[OK]" "[PASS]"
     "[ERROR]" "[DEBUG]" "[INFO]" "[WARN]" "[WARNING]" "[ERR]" "[FATAL]"
     "[TRACE]" "[FIXME]" "[TODO]" "[BUG]" "[NOTE]" "[HACK]" "[MARK]"
     "[FAIL]"  "<->" "<~>" "->" "<-" "~>" "<~" "=>"
     "!=" "!==" "!≡≡" "!=<" "#(" "#_" "#{" "#?" "##" "#_(" "#[" "%="
     "&%" "&&" "&+" "&-" "&/" "&=" "&&&" "$>" "(|" "*>" "++" "+++"
     "+=" "+>" "++=" "-<" "-<<" "-=" "->" "->>" "-->" "-+-"
     "-\\/" "-|>" "-<|" "->-" "-<-" "-|" "-||" "-|:" ".=" "//=" "/="
     "/==" "/-\\" "/-:" "/->" "/=>" "/-<" "/=<" "/=:" ":=" ":=" ":=>"
     ":-\\" ":=\\" ":-/" ":=/" ":-|" ":=|" ":|-" ":|=" "<$>" "<*"
     "<*>" "<+>" "<-" "<<=" "<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<"
     "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>" "<&>"
     "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<="
     "<->" "<!--" "<--" "<~<" "<==>" "<|-" "<||" "<<|" "<-<" "<-->"
     "<<==" "<==" "<-\\" "<-/" "<=\\" "<=/" "=<<" "==" "===" "==>"
     "=>" "=~" "=>>" "=~=" "==>>" "=>=" "=<=" "=<" "==<" "=<|" "=/"
     "=/=" "=/<" "=|" "=||" "=|:" ">-" ">=" ">>-" ">>=" ">=>" ">>^"
     ">>|" ">!=" ">->" ">==" ">=" ">/=" ">-|" ">=|" ">-\\" ">=\\"
     ">-/" ">=/" "?." "^=" "^^" "^<<" "^>>" "\\=" "\\==" "\\/-"
     "\\-/" "\\-:" "\\->" "\\=>" "\\-<" "\\=<" "\\=:" "|=" "|>=" "|>"
     "|+|" "|->" "|-->" "|=>" "|==>" "|>-" "|<<" "||>" "|>>" "|-"
     "||-" "||=" "|)" "|]" "|-:" "|=:" "|-<" "|=<" "|--<" "|==<" "~="
     "~>" "~~>" "~>>" "[[" "[|" "_|_" "]]"))
  (global-ligature-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil (Vim)
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
  (setq evil-shift-width tab-width)

  (global-set-key (kbd "C-s-]") 'evil-window-vsplit) ;; ]
  (global-set-key (kbd "s-ESC") 'evil-window-split) ;; [

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)

  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (define-key evil-ex-completion-map (kbd "TAB") 'completion-at-point)

  (define-key evil-visual-state-map (kbd ">") 'shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'shift-left-visual)

  (define-key evil-normal-state-map (kbd "C-c l") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "C-c .") 'embark-act))

(use-package evil-collection
  :ensure t
  :after (evil)
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init
   '(cider eglot vterm corfu magit dired vundo org vertico)))

(use-package evil-leader
  :ensure t
  :after (evil)
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  ;; Leaders
  (evil-leader/set-key
    "1"      (lambda () (interactive) (find-file "~/dotfiles/emacs"))
    "2"      (lambda () (interactive) (find-file "~/Sync/org/scratch.org"))
    "3"      (lambda () (interactive) (find-file "~/Sync/org/todo.org"))
    "4"      (lambda () (interactive) (find-file "~/Sync/org/journal.org"))
    "5"      (lambda () (interactive) (find-file "~/Sync/org/repl.org"))

    "<SPC>"  'embark-act
    "."      'er/expand-region
    ","      'consult-line-multi
    "e"      'eval-expression
    "q"      'quit-window
    "/"      'consult-line
    "x"      'execute-extended-command
    ";"      'consult-imenu
    "sh"     'eshell                    ; SHell
    "br"     'my-reload-buffer          ; BufferReload
    "bs"     'consult-buffer            ; BufferSwtich
    "bk"     'kill-buffer
    "ws"     'whitespace-mode
    "ln"     'display-line-numbers-mode

    "gg"     'magit
    "gd"     'magit-diff-unstaged
    "gs"     'magit-status
    "gb"     'magit-blame
    "gc"     'magit-commit
    "gl"     'magit-log-all
    "gP"     'magit-push-current-to-upstream
    "gp"     'magit-pull-from-upstream

    "p;"     'consult-imenu-multi
    "pp"     'consult-project-buffer
    "ps"     'project-switch-project
    "pf"     'project-find-file
    "pd"     'project-dired
    "pg"     'consult-git-grep
    "vt"     'multi-vterm

    "cc"     'cider-connect
    "ch"     'cider-repl-history
    "cb"     'cider-repl-clear-buffer
    "o."     'org-time-stamp
    "oe"     'org-export

    "fs"     'toggle-frame-fullscreen

    "XX"     'kill-emacs

    "u"      'vundo
    "|"      'evil-window-vsplit
    "_"      'evil-window-split
    "<left>" 'windmove-left
    "<right>"'windmove-right
    "<up>"   'windmove-up
    "<down>" 'windmove-down))

(use-package evil-cleverparens
  :ensure t
  :after (evil paredit)
  :diminish evil-cleverparens-mode
  :commands evil-cleverparens-mode)

(use-package expand-region
  :ensure t
  :bind (:map evil-visual-state-map
              ("." . 'er/expand-region)
              ("," . 'er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vertico, Prescient, Corfu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 1)
  :custom
  (vertico-cycle t))

;; Configure directory extension.
(use-package vertico-directory
  :after (vertico)
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function 'consult-completion-in-region))

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Popup completion-at-point
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator))

;; Add extensions
(use-package cape
  :ensure t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode)
  (setq completion-styles '(prescient basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :init
  (corfu-prescient-mode))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :init
  (vertico-prescient-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm :ensure t)
(use-package multi-vterm
  :after (vterm)
  :ensure t
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return)
  ;;https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359
  ;; .screenrc => termcapinfo xterm* ti@:te@
  ;; It makes wrapping after resize work
  (setq vterm-shell "screen")
  (setq vterm-keymap-exceptions nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming & Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation and code style
;; java/c/c++
(setq c-basic-offset 2)
;; web development
(setq javascript-indent-level 2)       ; javascript-mode
(setq js-indent-level 2)               ; js-mode
(setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
(setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
(setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
(setq css-indent-offset 2)             ; css-mode

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Code References

(use-package xref
  :ensure nil
  :bind
  ([remap xref-find-apropos] . xref-find-definitions)
  ([remap xref-find-definitions] . xref-find-definitions-other-window))

;;;; LSP Client
(use-package eglot
  :ensure t
  :defer t
  :hook ((clojure-mode . eglot-ensure))
  :config
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
                mode-line-misc-info))
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  :custom
  (eglot-autoshutdown t))

;; Web stuff
(use-package web-mode
  :ensure t
  :defer t
  :hook (web-mode . custom-web-mode-hook)
  :mode (("\\.html?\\'" . web-mode))
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
  (electric-pair-mode)
  (setq ess-r-backend 'lsp))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (electric-pair-mode))


;; Lisp
(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode)

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)

(defun enable-lisp-utils ()
  (paredit-mode)
  (evil-cleverparens-mode))

(defvar lisps
  '(emacs-lisp-mode
    lisp-mode
    ielm-mode
    cider-mode
    cider-repl-mode
    clojure-ts-mode
    clojure-mode))

(dolist (mode lisps)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'enable-lisp-utils))

(defun set-emacs-lisp-mode ()
  (let ((file-name (expand-file-name (buffer-file-name))))
    (when (or (string= file-name (expand-file-name "~/dotfiles/emacs"))
              (string= file-name (expand-file-name "~/.emacs")))
      (emacs-lisp-mode))))

(add-hook 'find-file-hook 'set-emacs-lisp-mode)

;; Clojure
(use-package cider
  :ensure t
  :commands cider-jack-in
  :hook ((clojure-ts-mode . cider-mode)
         (clojure-mode . cider-mode))
  :config
  (setq cider-eldoc-display-symbol-at-point nil
        cider-auto-select-error-buffer nil
        cider-repl-print-length 100
        cider-repl-display-help-banner nil
        cider-repl-use-pretty-printing t))

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
   `(term-color-white ((t (:foreground "#FAFAFA"))))
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
 '(custom-safe-themes
   '("801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" default))
 '(package-selected-packages
   '(js2-mode clojure-ts-mode tao-theme cider highlight-parentheses evil-cleverparens paredit aggressive-indent ess web-mode orderless corfu marginalia vertico magit expand-region evil-leader evil-collection ligature rainbow-mode vterm diminish direnv vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-key-binding ((t (:inherit default :background "grey96" :foreground "DarkBlue" :box (:line-width (-1 . -1) :color "grey80") :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#FCFCFC" :background "#616161" :inherit default)))))
