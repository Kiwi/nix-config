;;; -*- lexical-binding: t; -*-

;; I agree with this http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;; but oh well
(use-package undo-tree :init
  (add-hook 'after-init-hook 'global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; minibuffer, stop cursor going into it http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; it still happens sometimes, but oh well
(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; Tramp gives me problems opening large text files without this.
(setq tramp-copy-size-limit nil)

;; help me learn emacs keys
(use-package guru-mode :demand
  :config
  (guru-global-mode +1))

(provide '99workarounds)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
