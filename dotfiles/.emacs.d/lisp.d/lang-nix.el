;;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :init (add-to-list 'auto-mode-alist '("\\.nix?\\'" . nix-mode)))

(use-package helm-nixos-options :init
  (global-set-key (kbd "C-c C-S-n") 'helm-nixos-options))

(use-package company-nixos-options :demand :after company :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox)

;; flycheck find executables of checkers that would be only accessible via nix-shell
(setq flycheck-command-wrapper-function
      (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

;; allow haskell-mode to look for ghc in the current sandbox.
(setq haskell-process-wrapper-function
      (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
