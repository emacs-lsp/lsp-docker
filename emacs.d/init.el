(setq package-selected-packages
      '(;; lsp packages
        lsp-mode     ;; core
        lsp-ui       ;; ui stuff + flycheck support
        lsp-treemacs ;; more ui stuff
        company-lsp  ;; company support
        ccls         ;; ccls support
        helm-lsp     ;; helm support
        lsp-origami  ;; code folding support
        helm-lsp     ;; helm interation
        dap-mode     ;; debugger support
        yasnippet    ;; helpers

        ;; major modes not in core
        dockerfile-mode
        go-mode
        typescript-mode))

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

(setq lsp-log-io t)
(setq-default lsp-ui-sideline-show-hover t)
(require 'lsp-go)
(require 'lsp-html)
;; lsp configuration end

(yas-global-mode)
(ido-mode)
