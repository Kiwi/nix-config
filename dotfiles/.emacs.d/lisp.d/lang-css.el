;;; -*- lexical-binding: t; -*-

(use-package rainbow-mode)
(use-package css-mode
  :init
  (add-hook 'css-mode-hook 'prelude-css-mode-defaults)
  :config
  (setq css-indent-offset 2)

  (defun prelude-css-mode-defaults ()
    (rainbow-mode +1)))

(provide 'lang-css)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
