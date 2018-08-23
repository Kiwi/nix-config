;;; -*- lexical-binding: t; -*-

(use-package scss-mode
  :init
  (add-hook 'scss-mode-hook 'prelude-scss-mode-defaults)
  :config
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil)

  (defun prelude-scss-mode-defaults ()
    (prelude-css-mode-defaults)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
