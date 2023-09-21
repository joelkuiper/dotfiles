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
  "Revert-buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun my-open-wikipedia (search-term)
  "Open Wikipedia for the given SEARCH-TERM using eww."
  (interactive "sEnter search term: ")
  (eww (format "https://en.wikipedia.org/wiki/%s" search-term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  (setq-default
   ;; Mostly from https://github.com/angrybacon/dotemacs/blob/master/lisp/use-defaults.el
   locale-coding-system 'utf-8                       ;; Always, always, prefer UTF-8, anything else is insanity
   inhibit-startup-screen t                          ;; No startup screen
   inhibit-startup-message t                         ;; No startup message
   inhibit-startup-echo-area-message t               ;; No echo message
   initial-scratch-message nil                       ;; What's in the *scratch* buffer
   initial-major-mode 'fundamental-mode              ;; What major-mode Emacs opens with
   enable-recursive-minibuffers t                    ;; Use the minibuffer whilst in the minibuffer
   completion-cycle-threshold 1                      ;; TAB cycles candidates
   completions-detailed t                            ;; Show annotations
   tab-always-indent 'complete                       ;; Indent first then try completions
   indent-tabs-mode nil                              ;; Prefer spaces over tabs to indent
   tramp-terminal-type "tramp"                       ;; Tramp compatibility
   ad-redefinition-action 'accept                    ;; Silence warnings for redefinitions
   auto-save-list-file-prefix nil                    ;; Prevent tracking for auto-saves
   backup-by-copying t                               ;; Backups never overwrite original
   version-control t                                 ;; Use numeric versions for backups
   delete-old-versions t                             ;; Delete extra backups silently
   backup-directory-alist `(("." . "~/.backups"))    ;; Where are the backups?
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t)) ;; Auto save in tmp
   comment-multi-line t                              ;; Continue comments when filling
   create-lockfiles nil                              ;; Locks are more nuisance than blessing
   cursor-in-non-selected-windows nil                ;; Hide the cursor in inactive windows
   cursor-type '(hbar . 2)                           ;; Underline-shaped cursor
   custom-file null-device                           ;; Prevent littering
   custom-unlispify-menu-entries nil                 ;; Prefer kebab-case for titles
   custom-unlispify-tag-names nil                    ;; Prefer kebab-case for symbols
   delete-by-moving-to-trash t                       ;; Delete files to trash
   fill-column 80                                    ;; Set width for automatic line breaks
   gc-cons-threshold (* 8 1024 1024)                 ;; We're not using Game Boys anymore
   max-mini-window-height 10                         ;; Limit height for minibuffer transients
   mouse-yank-at-point t                             ;; Yank at point rather than pointer
   native-comp-async-report-warnings-errors 'silent  ;; Skip error buffers
   byte-compile-warnings '(not obsolete)             ;; Disable obsolete warnings when compiling
   warning-suppress-log-types '((comp) (bytecomp))   ;; Suppress compiler warning messages
   read-process-output-max (* 1024 1024)             ;; Increase read size for data chunks
   recenter-positions '(5 bottom)                    ;; Set re-centering positions
   ring-bell-function 'ignore                        ;; Silence error bells
   scroll-conservatively 101                         ;; Avoid recentering when scrolling far
   scroll-margin 5                                   ;; Add a margin when scrolling vertically
   select-enable-clipboard t                         ;; Merge system's and Emacs' clipboard
   sentence-end-double-space nil                     ;; Use a single space after dots
   show-help-function nil                            ;; Disable help text everywhere
   uniquify-buffer-name-style 'forward               ;; Uniquify buffer names
   use-short-answers t                               ;; Replace yes/no prompts with y/n
   vc-follow-symlinks t                              ;; Never prompt when visiting symlinks
   window-combination-resize t                       ;; Resize windows proportionally
   x-stretch-cursor t                                ;; Stretch cursor to the glyph width
   browse-url-browser-function 'eww-browse-url)      ;; Click hyperlink, open eww (or use ,wB)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable the erase-buffer function to clear buffer contents without restrictions.
  (put 'erase-buffer 'disabled nil)

  ;; Esc quits.
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Right click context menu.
  (context-menu-mode)

  ;; Zoom in/out (text scale) for my poor eyes (and presentations).
  (global-set-key (kbd "C-s--") 'text-scale-decrease)
  (global-set-key (kbd "C-s-=") 'text-scale-increase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility packages and setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensures that the Emacs shell environment matches the system's.
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; Provides undo/redo functionality.
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

;;;; Code References
(use-package xref
  :ensure nil)

;; Hides eldoc minor mode from the mode line.
(use-package eldoc
  :diminish eldoc-mode)

;; Hides auto-revert minor mode from the mode line.
(use-package autorevert
  :diminish auto-revert-mode)

;; Hides flymake mode from the mode line.
(use-package flymake
  :defer t
  :diminish flymake-mode)

;; Saves and restores command history over Emacs restarts.
(use-package savehist
  :ensure t
  :init (savehist-mode))

;; Winner winner, chicken ...
(use-package winner
  :config
  (winner-mode 1)
  (global-set-key (kbd "C-s-<left>") 'winner-undo)
  (global-set-key (kbd "C-s-<right>") 'winner-redo))

;; It moves between splits in buffers like the wind.
(use-package windmove
  :config
  (global-set-key (kbd "C-M-s-<right>") 'windmove-right)
  (global-set-key (kbd "C-M-s-<left>") 'windmove-left)
  (global-set-key (kbd "C-M-s-<down>") 'windmove-down)
  (global-set-key (kbd "C-M-s-<up>") 'windmove-up))

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Well it is nice to imagine a better world sometimes.
(use-package eww
  :defer t
  :ensure nil)

;; Recent files, distant memories
(use-package recentf
  :init
  (recentf-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme & Visual.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(defvar my-font)

(defun my-determine-font-height ()
  "Determine the font size based on the hostname."
  (let ((hostname (system-name)))
    (cond ((string-equal hostname "Aether") 160)
          ((string-equal hostname "Theseus") 140)
          (t 140)))) ; Default font size

(defun my-set-font-faces ()
  (let* ((main-font "PragmataPro Liga")
         (fallback "monospace")
         (font (if (x-list-fonts main-font) main-font fallback))
         (faces `(default fixed-pitch)))
    (dolist (face faces)
      (set-face-attribute
       face nil
       :family font
       :height (my-determine-font-height)))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my-set-font-faces))))
  (my-set-font-faces))

(use-package tao-theme
  :ensure t
  :init
  (load-theme 'tao-yang t))

(use-package spaceline
  :ensure t
  :custom-face
  (mode-line
   ((t (:foreground "#3C3C3C" :background "#DADADA"))))
  (powerline-active1 ((t (:foreground "#3C3C3C" :background "#DADADA"))))
  (powerline-active2 ((t (:foreground "#3C3C3C" :background "#F1F1F1"))))
  (powerline-inactive1
   ((t (:foreground "#3C3C3C" :background "#DADADA"))))
  (powerline-inactive2
   ((t (:foreground "#3C3C3C" :background "#DADADA"))))
  (mode-line-inactive
   ((t (:foreground "#3C3C3C" :background "#DADADA"))))
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-buffer-size-p nil
        spaceline-buffer-encoding-abbrev-p nil)
  :init
  (spaceline-emacs-theme))

;; You'll see what it does below, if not harfbuzz is broken.
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
  (setq
   evil-undo-system 'undo-redo
   evil-want-keybinding nil
   evil-shift-width 2)
  :config
  (evil-mode 1)
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

  (define-key evil-ex-completion-map (kbd "TAB") 'completion-at-point)

  (define-key evil-visual-state-map (kbd "g/") 'comment-or-uncomment-region)
  (define-key evil-visual-state-map (kbd ">") 'shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'shift-left-visual))

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init
   '(corfu cider eglot dired vterm magit eww vundo org vertico)))

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
    ;; File jump
    "1"      (lambda () (interactive) (find-file "~/dotfiles/emacs"))
    "2"      (lambda () (interactive) (find-file "~/Sync/org/todo.org"))
    "3"      (lambda () (interactive) (find-file "~/Sync/org/journal.org"))
    "4"      (lambda () (interactive) (find-file "~/Sync/org/scratch.org"))
    "5"      (lambda () (interactive) (find-file "~/Sync/org/repl.org"))

    ;; General
    "<SPC>"  'embark-act                       ;; Do something
    "."      'er/expand-region                 ;; See expand-region
    ","      'consult-imenu-multi              ;; Find a thing in all buffers
    ";"      'consult-imenu                    ;; Navigate buffer
    "'"      'consult-line-multi               ;; Find ALL the lines
    "/"      'consult-line                     ;; Like Vim / in current buffer
    "x"      'execute-extended-command         ;; eXecute
    ":"      'emoji-search                     ;; :melting-face: => 🫠
    "e"      'eval-expression                  ;; Eval
    ">"      'ffap                             ;; > FindFileAtPoint
    "<"      'consult-buffer                   ;; < Been there before
    "q"      'kill-current-buffer              ;; Quit
    "y"      'consult-yank-pop                 ;; Yank (well paste...)
    "c"      'consult-mode-command             ;; Command
    "r"      'consult-recent-file              ;; Recentf
    "u"      'vundo                            ;; Undo
    "?"      'eldoc                            ;; Help?
    "!"      'shell-command                    ;; Like [esc]:! in Vim
    "sh"     'term                             ;; SHell
    "vt"     'multi-vterm                      ;; VTerm
    "ws"     'whitespace-mode                  ;; WhiteSpace
    "wb"     'eww                              ;; WebBrowse
    "wB"     'browse-url-xdg-open              ;; WebBROWSE (with anger)
    "wi"     'my-open-wikipedia                ;; WIki
    "ln"     'display-line-numbers-mode        ;; LineNumbers
    "ff"     'find-file                        ;; FindFile
    "fg"     'consult-ripgrep                  ;; FindGrep
    "ai"     'gptel                            ;; ArtificialIntelligence
    "aai"    'gptel-menu                       ;; AnotherArtificialIntelligence
    "XX"     'kill-emacs                       ;; ...

    ;; Buffers
    "|"      'evil-window-vsplit               ;; :vsp
    "_"      'evil-window-split                ;; :sp
    "br"     'my-reload-buffer                 ;; BufferReload
    "bs"     'consult-buffer                   ;; BufferSwtich
    "bk"     'kill-buffer                      ;; BufferKill

    ;; Magit
    "gs"     'magit-status                     ;; GitStatus
    "gd"     'magit-diff-unstaged              ;; GitDiff
    "gl"     'magit-log-all                    ;; GitLog
    "gP"     'magit-push-current-to-upstream   ;; GitPUSH
    "gp"     'magit-pull-from-upstream         ;; GitPull

    ;; "Projects"
    "pb"     'consult-project-buffer           ;; ProjectBuffer
    "pg"     'consult-git-grep                 ;; Project(git)Grep
    "ps"     'project-switch-project           ;; ProjectSwitch
    "pf"     'project-find-file                ;; ProjectFind
    "pd"     'project-dired                    ;; ProjectDired
    "p!"     'project-shell-command            ;; Like ! but p!

    ;; Org mode
    "o."     'org-time-stamp
    "oe"     'org-export))

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
  (define-key evil-normal-state-map (kbd "C-c .") 'embark-act)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun my-corfu-quit-and-escape ()
  "Call `corfu-quit' and then return to Normal State."
  (interactive)
  (call-interactively 'corfu-quit)
  (evil-normal-state))

;; Popup completion-at-point
;; https://github.com/minad/corfu
(use-package corfu
  :bind
  (:map corfu-map
        (("<escape>" . my-corfu-quit-and-escape)
         ;; Free the RET key for less intrusive behavior.
         ;; ("RET" . nil)
         ("SPC" . corfu-insert-separator)))
  :init
  (global-corfu-mode)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))

;; https://github.com/radian-software/prescient.el
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :init
  (vertico-prescient-mode))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient eglot)
  :init
  (corfu-prescient-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package term
  :custom-face
  ;; vterm colors customizations with :inherit background
  (term-color-white ((t (:foreground "#FAFAFA"))))
  (term-color-black ((t (:foreground "#241f31"))))
  (term-color-red ((t (:foreground "#9C3328"))))
  (term-color-green ((t (:foreground "#556B2F"))))
  (term-color-yellow ((t (:foreground "#B8860B"))))
  (term-color-blue ((t (:foreground "#4A708B"))))
  (term-color-magenta ((t (:foreground "#8B008B"))))
  (term-color-cyan ((t (:foreground "#68BFBF")))))

(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return)
  ;; https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359
  ;; .screenrc => termcapinfo xterm* ti@:te@
  ;; It makes wrapping after resize work but you now have to deal with screens everywhere
  ;; (setq vterm-shell "screen")
  )

(use-package multi-vterm
  :after vterm
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ChatGPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/karthink/gptel
;; set `machine api.openai.com login apikey password TOKEN` in ~/.authinfo
;; Also see https://github.com/karthink/gptel/wiki
(use-package gptel
  :ensure t
  :bind (:map gptel-mode-map
              ("C-<return>" . gptel-send))
  :config
  (define-key evil-visual-state-map (kbd "ai") 'gptel-send)
  (define-key evil-visual-state-map (kbd "aai") 'gptel-menu)
  (setq gptel-default-mode 'org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming & Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :ensure t
  :init
  (setq
   whitespace-line-column 80
   whitespace-style
   '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))

  (add-hook
   'before-save-hook
   (lambda ()
     (whitespace-cleanup)
     (delete-trailing-whitespace))))

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)

;; Colors hex codes of colors their colors in buffers
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((prog-mode . rainbow-mode)
         (text-mode . rainbow-mode)))

;; LSP Client
(use-package eglot
  :ensure t
  :defer t
  :hook ((clojure-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config

  (setq completion-category-overrides '((eglot (styles prescient))))
  ;; https://github.com/minad/corfu/wiki#continuously-update-the-candidates
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my-eglot-capf ()
    (setq-local completion-at-point-functions
                (list
                 ;; Bit of a hack to get Cider and Eglot to play
                 (when (and (boundp 'cider-mode) cider-mode)
                   (cape-super-capf #'cider-complete-at-point))
                 (cape-super-capf #'eglot-completion-at-point)
                 #'cape-dabbrev
                 #'cape-file)))

  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)

  ;;  Is there a way of making the the modeline not flicker? #1283
  ;; https://github.com/joaotavora/eglot/discussions/1283
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
                mode-line-misc-info))

  (setq
   eldoc-echo-area-use-multiline-p nil
   eglot-extend-to-xref t
   eglot-connect-timeout 6000
   eglot-confirm-server-initiated-edits nil)

  (define-key evil-normal-state-map (kbd "C-c l") 'eglot-code-actions)
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp"))))

;; https://github.com/joaotavora/eglot/issues/661
(use-package jarchive :ensure t
  :init
  (jarchive-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-electric-tag-pair-mode)
  :custom
  (sgml-basic-offset 2))

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

;; Javascript (as little as humanly possible)
(use-package js2-mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :hook ((js2-mode . electric-pair-mode)
         (js2-mode . electric-indent-mode))
  :config
  (setq js-indent-level 2
        javascript-indent-level 2))

;; Emacs Speaks Statistics (R) 🏴‍☠️
(use-package ess
  :ensure t
  :defer t
  :hook ((R-mode . electric-pair-mode)
         (R-mode . electric-indent-mode))
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :config
  (setq ess-eval-visibly 'nowait
        ess-r-backend 'lsp))

;; Lisp
(use-package paredit
  :ensure t
  :defer t
  :hook
  ((lisp-mode . paredit-mode)
   (cider-mode . paredit-mode)
   (cider-repl-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode))
  :diminish paredit-mode
  :commands paredit-mode)

(use-package evil-cleverparens
  :ensure t
  :hook (paredit-mode . evil-cleverparens-mode)
  :diminish evil-cleverparens-mode
  :commands evil-cleverparens-mode)

(use-package aggressive-indent
  :ensure t
  :hook (paredit-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode)

(use-package emacs-lisp-mode
  :mode (("emacs\\'" . emacs-lisp-mode)
         ("\\.emacs\\'" . emacs-lisp-mode)))

;; Clojure
(use-package cider
  :ensure t
  :defer t
  :commands cider-jack-in
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-eldoc-display-symbol-at-point nil
        cider-auto-select-error-buffer nil
        cider-repl-print-length 100
        cider-repl-display-help-banner nil
        cider-repl-use-pretty-printing t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing & Blogging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :defer t
  :hook ((org-mode . visual-line-mode)
         (org-mode . flyspell-mode))
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-directory "~/Sync/org/"
        ;; https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
        org-src-fontify-natively t ;; font-lock src-block
        org-startup-folded 'content) ;; collapse all trees

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

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :defer t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-key-binding ((t (:inherit default :background "grey96" :foreground "DarkBlue" :box (:line-width (-1 . -1) :color "grey80") :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#FCFCFC" :background "#616161" :inherit default)))))
