;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'prelude-haskell-mode-defaults)
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (defun prelude-haskell-mode-defaults ()
    (subword-mode +1)
    (eldoc-mode +1)
    (haskell-indentation-mode +1)
    (interactive-haskell-mode +1)))

(provide '60lang-haskell)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
