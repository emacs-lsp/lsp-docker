(setq package-selected-packages
      '(;; lsp packages
        ;; core
        lsp-mode

        ;; ui stuff + flycheck support
        lsp-ui

        ;; more ui stuff
        lsp-treemacs

        ;; company support
        company-lsp

        ;; java support
        lsp-java

        ;; ms python support
        lsp-python-ms

        ;; haskell support
        lsp-haskell

        ;; ccls support
        ccls

        ;; helm support
        helm-lsp

        ;; code folding support
        lsp-origami

        ;; helm interation
        helm-lsp

        ;; debugger support
        dap-mode

        ;; helpers
        yasnippet

        ;; major modes not in core
	dockerfile-mode go-mode typescript-mode))

        ;;rust-mode php-mode  kotlin-mode scala-mode flycheck erlang csharp-mode
        ;;posframe vue-mode elixir-mode elm-mode powershell go-mode rjsx-mode))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

;; lsp configuration begin
(with-eval-after-load 'lsp-mode
  (require 'yasnippet))

(add-hook 'prog-mode-hook 'lsp-deferred)
(add-hook 'vue-mode-hook 'lsp-deferred)

(setq lsp-log-io t)
(setq-default lsp-ui-sideline-show-hover t)
(require 'lsp-go)
(require 'lsp-html)
;; lsp configuration end

;; non lsp related stuff
(ido-mode)

(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd ";") nil)
  (define-key java-mode-map (kbd "(") nil))

;; (require 'lsp-python-ms)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode lsp-mode lsp-ui lsp-treemacs company-lsp lsp-java lsp-python-ms lsp-haskell ccls helm-lsp lsp-origami helm-lsp dap-mode yasnippet rust-mode php-mode typescript-mode kotlin-mode scala-mode flycheck erlang csharp-mode posframe vue-mode elixir-mode elm-mode powershell go-mode rjsx-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
