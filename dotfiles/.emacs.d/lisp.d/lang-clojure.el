;;; -*- lexical-binding: t; -*-

(use-package clojure-snippets)
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'prelude-clojure-mode-defaults)
  :config
  (defun prelude-clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'prelude-lisp-coding-hook)))

(use-package cider
  :init
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)
  :config
  (setq nrepl-log-messages t)

  (defun prelude-cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'prelude-interactive-lisp-coding-hook)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
