;;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix?\\'" . nix-mode))
  :config)

;; TODO https://github.com/shlevy/nix-buffer
;; https://github.com/travisbhartwell/nix-emacs

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
