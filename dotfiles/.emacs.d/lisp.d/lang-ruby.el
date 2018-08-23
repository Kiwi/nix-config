;;; -*- lexical-binding: t; -*-

(use-package inf-ruby)
(use-package yari)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

  (define-key 'help-command (kbd "R") 'yari)
  (defun prelude-ruby-mode-defaults ()
    (inf-ruby-minor-mode +1)
    ;; CamelCase aware editing operations
    (subword-mode +1))

  (add-hook 'ruby-mode-hook 'prelude-ruby-mode-defaults))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
