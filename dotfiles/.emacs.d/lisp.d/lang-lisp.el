;;; -*- lexical-binding: t; -*-

;; some shared keys and functions for all lisp modes.

(use-package rainbow-delimiters)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (prelude-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-[") (prelude-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (prelude-wrap-with "\""))

;; a great lisp coding hook
(defun prelude-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq prelude-lisp-coding-hook 'prelude-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun prelude-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq prelude-interactive-lisp-coding-hook 'prelude-interactive-lisp-coding-defaults)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
