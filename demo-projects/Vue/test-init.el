(require 'package)
(package-initialize)
(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t)

;;(global-font-lock-mode 0)

;;; Vue mode, based on mmm-mode -- set up for .vue files (html/css/script)
(use-package vue-mode
  :mode "\\.vue$"
  :config
  (setq mmm-submode-decoration-level 0) ; don't color background of sub-modes
  )

(use-package yasnippet
  :config (yas-global-mode))

(use-package lsp-mode
  :commands lsp
  :hook ((vue-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil
        lsp-log-io t
        lsp-trace t
        lsp-print-performance t
        lsp-response-timeout 20
        )
  )
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        )
  )
(use-package company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates 'auto ; don't use nil or t
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex
        )
  )
