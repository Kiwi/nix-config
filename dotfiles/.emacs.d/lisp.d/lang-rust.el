;;; -*- lexical-binding: t; -*-

(use-package racer)
(use-package flycheck-rust)
(use-package cargo)
(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook 'flycheck-rust-setup)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'prelude-rust-mode-defaults)
  :mode "\\.rs\\'"
  :commands rust-mode
  :config
  (setq rust-format-on-save t)

  (defun prelude-rust-mode-defaults ()
    (local-set-key (kbd "C-c C-d") 'racer-describe)
    ;; CamelCase aware editing operations
    (subword-mode +1)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
