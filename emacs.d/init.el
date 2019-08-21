(setq package-selected-packages '(lsp-mode lsp-ui company-lsp yasnippet lsp-java lsp-python-ms lsp-haskell helm-lsp lsp-treemacs dap-mode lsp-origami helm-lsp))

                                        ; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd ";") nil)
  (define-key java-mode-map (kbd "(") nil))

(add-hook 'prog-mode-hook 'lsp)

(ido-mode)
