;;; Init --- my emacs configuration
;;; Commentary:
;;; Code:

;; GUI


(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defun skrat/disable-scroll-bars (frame)
  "Disable scroll bars on new FRAME."
  (modify-frame-parameters
   frame '((vertical-scroll-bars . nil)
	   (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'skrat/disable-scroll-bars)

;; Settings


(savehist-mode 1)
(desktop-save-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq debug-on-error t)
(setq vc-follow-symlinks t)
(setq mouse-autoselect-window t)
(setq-default indent-tabs-mode nil)
(setq eldoc-idle-delay 0.5)
(fset 'yes-or-no-p #'y-or-n-p)
(advice-add 'risky-local-variable-p :override #'ignore)
(put 'dired-find-alternate-file 'disabled nil)

;; Backups and autosaves in one location

(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; Email

(require 'cl)
(lexical-let* ((maildir "~/Mail")
               (mu4edir "/usr/share/emacs/site-lisp/mu4e")
               (account "dusan@struna.me"))
  (when (and (file-directory-p maildir)
             (file-directory-p mu4edir))
    (add-to-list 'load-path mu4edir)
    (load-library "mu4e")
    (require 'mu4e)
    (setq mu4e-completing-read-function 'ivy-completing-read)
    (setq mu4e-alert-interesting-mail-query
          (concat "flag:unread maildir:/" account "/INBOX"))
    (setq mu4e-maildir maildir)
    (setq message-kill-buffer-on-exit t)
    (setq mu4e-context-policy 'pick-first)
    (setq mu4e-confirm-quit nil)
    (defun skrat/remove-nth-element (nth list)
      (if (zerop nth) (cdr list)
        (let ((last (nthcdr (1- nth) list)))
          (setcdr last (cddr last))
          list)))
    (setq mu4e-marks (skrat/remove-nth-element 5 mu4e-marks))
    (add-to-list 'mu4e-marks
                 '(trash
                   :char ("d" . "▼")
                   :prompt "dtrash"
                   :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                   :action (lambda (docid msg target)
                             (mu4e~proc-move docid
                                             (mu4e~mark-check-target target) "-N"))))
    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name account
              :match-func (lambda (msg)
                            (when msg
                              (string-prefix-p
                               (concat "/" account)
                               (mu4e-message-field msg :maildir))))
              :vars `((user-mail-address  . "dusan@struna.me")
                      (user-full-name     . "Dusan Maliarik")
                      (mu4e-drafts-folder . ,(concat "/" account "/Drafts"))
                      (mu4e-sent-folder   . ,(concat "/" account "/Sent"))
                      (mu4e-trash-folder  . ,(concat "/" account "/Trash"))
                      (mu4e-refile-folder . ,(concat "/" account "/Archive"))))))))

;; Customs stuff

(setq custom-file "~/.emacs.d/custom.el")
(shell-command (format "touch %s" custom-file))
(load custom-file)

;; Whitespace nuking

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Packages


(require 'color)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package quelpa
  :ensure t
  :init
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package :ensure t)

;; Theme


(use-package gotham-theme
  :ensure t
  :config
  (load-theme 'gotham t))

(global-hl-line-mode)

(set-face-attribute
 'default  nil
 :family  "DejaVu Sans Mono"
 :height   90
 :weight  'normal
 :width   'normal)

(defun skrat/darken (face attribute pct)
  "Darker color of the FACE ATTRIBUTE by PCT."
  (color-darken-name (face-attribute face attribute) pct))

(defun skrat/lighten (face attribute pct)
  "Lighter color of the FACE ATTRIBUTE by PCT."
  (color-lighten-name (face-attribute face attribute) pct))

;; Functions


(defun skrat/counsel-ag-at-point ()
  "Ag symbol at point."
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

(defun skrat/gradle-installDebug ()
  "Android: Run gradle installDebug."
  (interactive)
  (gradle-execute "installDebug"))

(defun skrat/ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "|" (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat " " str))
   cands
   "\n"))

(defun skrat/tide-if-tsx ()
  "Tide mode if filename ends with .tsx."
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (tide-setup)
    (eldoc-mode)))

(defun skrat/tide ()
  "Setup tide mode."
  (tide-setup)
  (eldoc-mode +1))

(defun skrat/toggle-comment (beg end)
  "Comment or uncomment thing BEG END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (comment-or-uncomment-region beg end)
    (comment-line 1)))

(defun skrat/flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (concat
   " " flycheck-mode-line-prefix
   (pcase (or status flycheck-last-status-change)
     (`not-checked "")
     (`no-checker "-")
     (`running (propertize "*" 'face '(:foreground "green")))
     (`errored (propertize "!" 'face '(:foreground "red")))
     (`finished
      (let-alist (flycheck-count-errors flycheck-current-errors)
        (if (or .error .warning)
	    (concat
	     ":"
	     (if (= 0 (or .error 0)) "0"
	       (propertize (int-to-string .error) 'face '(:foreground "red")))
	     "/"
	     (if (= 0 (or .warning 0)) "0"
	       (propertize (int-to-string .warning) 'face '(:foreground "yellow"))))
          "")))
     (`interrupted ".")
     `suspicious)))

;; Core


(use-package dash
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package smart-mode-line
  :ensure t
  :config (smart-mode-line-enable))

(use-package diminish
  :ensure t)

(use-package delight
  :ensure t
  :delight
  (eldoc-mode " ♦")
  (auto-revert-mode)
  (undo-tree-mode))

(use-package general
  :ensure t
  :bind*
  (("M-[" . previous-buffer)
   ("M-]" . next-buffer))
  :config
  (general-evil-setup t)
  (general-create-definer leader-def
   :states '(normal insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-c")
  (general-create-definer local-def
   :states '(normal insert emacs visual)
   :prefix ","
   :non-normal-prefix "C-,")
  (leader-def
    "`"   '(save-buffer :which-key "write")
    "ESC" 'ivy-resume
    "TAB" '(other-window :which-key "other")
    "RET" 'make-frame
    "b"  '(nil :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill")
    "bj" '(next-buffer :which-key "next")
    "bk" '(previous-buffer :which-key "prev")
    "f"  '(nil :which-key "file")
    "r"  '(nil :which-key "refactor")
    "s"  '(nil :which-key "symbol")
    "t"  '(nil :which-key "toggle")
    "t SPC" '(whitespace-mode :which-key "whitespace")
    "w"  '(nil :which-key "window")
    "w1" '(delete-other-windows :which-key "max")
    "wd" '(delete-window :which-key "kill"))
  (local-def
    ";"  '(skrat/toggle-comment :which-key "comment")
    "e"  '(nil :which-key "eval")
    "eb" '(eval-buffer :which-key "buffer")
    "ee" '(eval-last-sexp :which-key "last-sexp")
    "er" '(eval-region :which-key "region")
    "ef" '(eval-defun :which-key "defun")
    "h"  '(nil :which-key "help")
    "hm" 'man
    "s"  '(nil :which-key "symbol")))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
	      ("C-'" . ivy-avy)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)) ; C-' to ivy-avy
  :general
  (leader-def
    "bb" '(ivy-switch-buffer :which-key "switch"))
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-ignore-buffers '("\*.+\*"))
  (ivy-height 20)
  (ivy-count-format "(%d/%d) ")
  ;; (ivy-format-function 'skrat/ivy-format-function-arrow)
  (ivy-display-style 'fancy))

(use-package ivy-hydra
  :ensure t)

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode 1))

(use-package smex
  ;; this enabled recent entries in ivy
  :ensure t)

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . skrat/counsel-ag-at-point) ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  :general
  (leader-def
    "ff" '(counsel-find-file :which-key "find")
    "sj" '(counsel-semantic-or-imenu :which-key "jump"))
  :config
  (counsel-mode +1))

(use-package avy
  :general
  (leader-def
    "SPC" '(avy-goto-word-or-subword-1 :which-key "avy")))

(use-package swiper
  :general
  (:states '(normal)
   (kbd "*") (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))
   (kbd "#") (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word))))))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 1.0)
  :diminish which-key-mode)

(use-package company
  :custom
  (company-idle-delay 0)
  (company-tooltip-align-annotations 't)
  :config
  (global-company-mode 1)
  :diminish company-mode)

(use-package flycheck
  :config
  (global-flycheck-mode +1)
  :custom
  (flycheck-mode-line-prefix "Σ")
  (flycheck-mode-line '(:eval (skrat/flycheck-mode-line-status-text))))

(use-package fill-column-indicator
  :config
  (setq fci-rule-color (skrat/lighten 'default :background 10))
  (leader-def
    "tc" '(fci-mode :which-key "fci")))

(use-package evil-anzu
  :general
  (local-def
   "se" '(anzu-query-replace-at-cursor-thing :which-key "edit"))
  :diminish anzu-mode
  :config
  (global-anzu-mode +1))

(use-package focus
  :general
  (:modes '(normal)
   ;; "<tab>" 'focus-next-thing
   ;; "<backtab>" 'focus-prev-thing
   )
  (leader-def
    "tf" 'focus-mode))

(use-package highlight-symbol
  :general
  (leader-def
    "th" '(highlight-symbol-mode :which-key "highlight"))
  :delight
  (highlight-symbol-mode)
  :custom
  (highlight-symbol-idle-delay 0.5)
  :config
  (set-face-attribute
   'highlight-symbol-face nil :background (skrat/lighten 'default :background 10))
  :hook
  ((prog-mode . highlight-symbol-mode)))

(use-package helpful
  :general
  (local-def
   "hh" 'helpful-at-point)
  :config
  (general-def
   :states '(normal)
   :keymap helpful-mode-map
   "q" 'delete-window))

(use-package form-feed
  :diminish form-feed-mode
  :custom-face
  (form-feed-line ((t (:strike-through "#091F2E"))))
  :hook
  ((emacs-lisp-mode . form-feed-mode)))

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-generic-command "fd . -0")
  :general
  (leader-def
    "p" '(projectile-command-map :which-key "project"))
  :delight
  (projectile-mode)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after (projectile)
  :general
  (leader-def
    "ss" '(counsel-projectile-ag :which-key "ag"))
  :config
  (counsel-projectile-mode 1))

(use-package frame-fns
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))

(use-package frame-cmds
  :quelpa (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds"))

(use-package zoom-frm
  :after (frame-fns frame-cmds)
  :quelpa (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm")
  :bind*
  (("C-=" . 'zoom-frm-in)
   ("C--" . 'zoom-frm-out)))

;; Git


(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :general
  (leader-def
    "g" '(nil :which-key "git")
    "gs" '(magit-status :which-key "status")
    "gb" '(magit-blame :which-key "blame")))

(use-package evil-magit
  :after (magit))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (set-face-attribute 'diff-hl-change nil :foreground "#edb443")
  (set-face-attribute 'diff-hl-change nil :background (skrat/darken 'diff-hl-change :foreground 50))
  (set-face-attribute 'diff-hl-insert nil :background (skrat/darken 'diff-hl-insert :foreground 30))
  (set-face-attribute 'diff-hl-delete nil :background (skrat/darken 'diff-hl-delete :foreground 30))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :general
  ("M-n" 'diff-hl-next-hunk
   "M-m" 'diff-hl-previous-hunk)
  (leader-def
    "gr" '(diff-hl-revert-hunk :which-key "revert-hunk")))

(use-package evil-goggles
  :after (evil)
  :custom
  (evil-goggles-blocking-duration 0.05)
  :config
  (set-face-attribute 'evil-goggles-delete-face nil :background (color-darken-name "red3" 30))
  (set-face-attribute 'evil-goggles-paste-face nil :background (color-darken-name "#edb443" 50)))

;; LISP


(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook
  ((clojure-mode-hook . eldoc-mode)))

(use-package cider
  :after (clojure-mode)
  :delight
  (cider-mode " cider")
  (cider-auto-test-mode " AT")
  :hook
  ((clojure-mode . paredit-mode)
   (cider-repl-mode-hook . eldoc-mode)
   (cider-repl-mode-hook . paredit-mode))
  :custom
  (cider-prompt-for-symbol nil)
  (cider-save-file-on-load nil)
  :general
  (leader-def clojure-mode-map
    "c" '(nil :which-key "cider")
    "ce" '(cider-enlighten-mode :which-key "enlighten")
    "ci" '(cider-jack-in-cljs :which-key "jack-in-cljs")
    "cj" '(cider-jack-in :which-key "jack-in")
    "cn" '(cider-repl-set-ns :which-key "repl-set-ns")
    "cr" '(cider-ns-refresh :which-key "ns-refresh")
    "cq" '(cider-quit :which-key "quit"))
  (local-def clojure-mode-map
    "eb" '(cider-eval-buffer :which-key "buffer")
    "ed" '(cider-debug-defun-at-point :which-key "debug-defun")
    "ee" '(cider-eval-sexp-at-point :which-key "sexp-at-point")
    "ef" '(cider-eval-defun-at-point :which-key "defun-at-point")
    "ep" '(cider-pprint-eval-defun-at-point :which-key "pp-defun-at-point")
    "er" '(cider-eval-region :which-key "region")
    "hh" '(cider-doc :which-key "doc")
    "hj" '(cider-javadoc :which-key "javadoc"))
  (:keymaps 'clojure-mode-map
   "M-RET" 'cider-eval-defun-at-point
   [(meta shift return)] 'cider-pprint-eval-defun-at-point)
  (:keymaps 'cider-repl-mode-map
   "C-l" '(cider-repl-clear-buffer :which-key "clear REPL buffer"))
  (:keymaps 'cider-popup-buffer-mode-map
   :modes '(normal)
   "q" 'delete-window))

(use-package eval-sexp-fu
  :ensure t)

;; (use-package cider-eval-sexp-fu
;;   :ensure t)

(use-package clj-refactor
  :hook ((clojure-mode-hook . clj-refactor-mode)
	 (clojure-mode-hook . yas-minor-mode))
  :general
  (local-def clojure-mode-map
    "rt" '(nil :which-key "thread")
    "rtf" '(clojure-thread-first-all :which-key "first-all")
    "rtl" '(clojure-thread-last-all :which-key "first-all")))

(use-package paredit
  :delight
  (paredit-mode " ()")
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (eshell-mode . paredit-mode)
   (eval-expression-minibuffer-setup-hook . paredit-mode)
   (eval-expression-minibuffer-setup-hook . eldoc-mode)))

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (clojure-mode    . rainbow-delimiters-mode)))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (mu4e-alert-enable-mode-line-display)
  (defun skrat/refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))
  (run-with-timer 0 60 'skrat/refresh-mu4e-alert-mode-line))

(use-package highlight
  :general
  (local-def
    "thl" '(hlt-highlight-lines :which-key "highlight line")
    "thu" '(hlt-unhighlight-region :which-key "unhighlight")))

(use-package yasnippet)

;; Rest


(use-package gradle-mode
  :general
  (local-def
    "bi" '(skrat/gradle-installDebug :which-key "gradle-installDebug")))

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package tex
  :mode "\\.tex\\'"
  :ensure auctex)

(use-package latex-preview-pane
  :after (tex)
  :custom
  (latex-preview-pane-use-frame t))

(use-package web-mode
  :mode "\\.\\(html\\|tsx\\)\\'")

(use-package tide
  :mode ("\\.\\(ts\\|tsx\\)\\'" . typescript-mode)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . skrat/tide)
	 (web-mode-hook . skrat/tide-if-tsx)
         ;(before-save . tide-format-before-save)
	 )
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package omnisharp
  :mode ("\\.cs\\'" . csharp-mode)
  :after (company flycheck)
  :hook ((csharp-mode-hook . omnisharp-mode))
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (setq omnisharp-server-executable-path "/opt/omnisharp-roslyn/OmniSharp.exe"))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

;; C++

;; (use-package rtags
;;   :config
;;   (setq rtags-completions-enabled t)
;;   (setq rtags-autostart-diagnostics t)
;;   (setq rtags-verify-protocol-version nil)
;;   (rtags-enable-standard-keybindings)
;;   :general
;;   (leader-def c-mode-base-map
;;     "r" '(rtags-mode-map :which-key "RTags")))

;; (use-package company-rtags
;;   :after (rtags)
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list
;;       'company-backends 'company-rtags)))

;; (use-package irony
;;   :preface
;;   (defun skrat/irony-mode-hook ()
;;     "Blah blah."
;;     ;; (define-key irony-mode-map [remap completion-at-point]
;;     ;;   'irony-completion-at-point-async)
;;     ;; (define-key irony-mode-map [remap complete-symbol]
;;     ;;   'irony-completion-at-point-async)
;;     )
;;   :hook ((c++-mode . irony-mode)
;;          (c-mode . irony-mode)
;;          (irony-mode . irony-cdb-autosetup-compile-options)
;;          (irony-mode . skrat/irony-mode-hook))
;;   :general
;;   (leader-def irony-mode-map
;;     "d" '(nil :which-key "debug")
;;     "dg" 'gdb
;;     "dd" 'gud-break
;;     "dx" 'gud-remove
;;     "dr" 'gud-run
;;     "dc" 'gud-cont
;;     "dn" 'gud-next
;;     "dl" 'gdb-display-locals-for-thread))

;; (use-package irony-eldoc
;;   :after (irony)
;;   :hook (irony-mode . irony-eldoc))

;; (use-package counsel-irony
;;   :after (irony)
;;   :preface
;;   (defun skrat/counsel-irony-mode-hook ()
;;     "My irony-mode hook."
;;     (define-key irony-mode-map
;;       [remap completion-at-point] 'counsel-irony)
;;     (define-key irony-mode-map
;;       [remap complete-symbol] 'counsel-irony)
;;     )
;;   :hook (irony-mode . skrat/counsel-irony-mode-hook))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; (use-package cmake-ide
;;   :after (projectile rtags)
;;   :hook (c++-mode . skrat/cmake-ide-find-project)
;;   :preface
;;   (defun skrat/cmake-ide-find-project ()
;;     "Finds the directory of the project for cmake-ide."
;;     (with-eval-after-load 'projectile
;;       ;; (setq cmake-ide-project-dir (projectile-project-root))
;;       (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
;;     (setq cmake-ide-compile-command
;;           (concat "cd " cmake-ide-build-dir
;;                   " && cmake .. && make"))
;;     (cmake-ide-load-db))
;;   (defun skrat/switch-to-compilation-window ()
;;     "Switches to the *compilation* buffer after compilation."
;;     (other-window 1))
;;   :bind ([remap comment-region] . cmake-ide-compile)
;;   :init (progn (require 'rtags)
;;                (cmake-ide-setup))
;;   :config (advice-add 'cmake-ide-compile :after #'skrat/switch-to-compilation-window))

;; (use-package eglot
;;   :ensure t
;;   :hook ((c++-mode . eglot-ensure)
;;          (c++-mode . eldoc-mode))
;;   :config
;;   (require 'eglot)
;;   (setq company-backends
;;         (cons 'company-capf
;;               (remove 'company-capf company-backends)))
;;   (add-to-list 'eglot-server-programs '((c++ mode c-mode) . (eglot-cquery "clangd"))))

;; ---

;; (use-package counsel-etags
;;   :ensure t)

;; (use-package modern-cpp-font-lock
;;   :hook ((c++-mode . modern-c++-font-lock-mode))
;;   :diminish modern-c++-font-lock-mode)

;; (use-package ycmd
;;   :hook ((c++-mode . ycmd-mode)
;;          (ycmd-mode . ycmd-setup-completion-at-point-function)
;;          (ycmd-mode . ycmd-eldoc-setup))
;;   :config
;;   (require 'ycmd)
;;   (require 'ycmd-eldoc)
;;   (set-variable 'ycmd-server-command '("python" "/usr/share/ycmd/ycmd"))
;;   (set-variable 'ycmd-global-config "/home/skrat/.config/ycm-extra-conf.py")
;;   (set-variable 'ycmd-extra-conf-whitelist '("~/Workspace/*" "~/Workspace/renoworks/*" "~/.config/ycm-extra-conf.py"))
;;   :preface
;;   (defun ycmd-setup-completion-at-point-function ()
;;     "Setup `completion-at-point-functions' for `ycmd-mode'."
;;     (add-hook 'completion-at-point-functions
;;               #'ycmd-complete-at-point nil :local)))

;; (use-package company-ycmd
;;   :ensure t
;;   :after (ycmd)
;;   :config
;;   (company-ycmd-setup))

;; ---

(use-package lsp-mode
  :hook (c++-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package dap-mode :commands dap-mode)
;; (setq-local
;;  company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
;;                          company-preview-if-just-one-frontend
;;                          company-echo-metadata-frontend))

(provide '.emacs)
