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
        rust-mode php-mode typescript-mode kotlin-mode scala-mode flycheck erlang csharp-mode
        posframe vue-mode elixir-mode elm-mode powershell go-mode))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

;; lsp configuration begin
(with-eval-after-load 'lsp-mode
  (require 'yasnippet))

(add-hook 'prog-mode-hook 'lsp)
(add-hook 'vue-mode-hook 'lsp)
;; lsp configuration end

;; non lsp related stuff
(ido-mode)

(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd ";") nil)
  (define-key java-mode-map (kbd "(") nil))

;; (require 'lsp-python-ms)
