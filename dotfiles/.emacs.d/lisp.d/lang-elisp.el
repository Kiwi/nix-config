;;; -*- lexical-binding: t; -*-

;; Make some cool libs available for Emacs hackers. (Because why not.)
(require 'seq)
(require 'subr-x)
(require 'cl)
(use-package dash)
(use-package ht)
(use-package s)
(use-package a)

(use-package elisp-slime-nav :init
  ;; enable elisp-slime-nav-mode
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-hook)
  (eldoc-mode +1)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (prelude-conditional-emacs-lisp-checker))

(add-hook 'emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(add-hook 'ielm-mode-hook 'prelude-ielm-mode-defaults)

(with-eval-after-load "ielm"
  (define-key ielm-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key ielm-map (kbd "M-\"") (prelude-wrap-with "\"")))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'lang-elisp)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
