;;; -*- lexical-binding: t; -*-

(use-package cc-mode :config
  (defun prelude-c-mode-common-defaults ()
    (setq c-default-style "k&r"
          c-basic-offset 4)
    (c-set-offset 'substatement-open 0))

  ;; this will affect all modes derived from cc-mode, like
  ;; java-mode, php-mode, etc
  (add-hook 'c-mode-common-hook 'prelude-c-mode-common-defaults)

  (defun prelude-makefile-mode-defaults ()
    (whitespace-toggle-options '(tabs))
    (setq indent-tabs-mode t ))

  (add-hook 'makefile-mode-hook 'prelude-makefile-mode-defaults))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
