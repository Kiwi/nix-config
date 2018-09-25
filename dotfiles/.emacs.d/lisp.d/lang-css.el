;;; -*- lexical-binding: t; -*-

;; Adapted to my config from "Prelude". :)
;; Please check them out and give them github stars. https://github.com/bbatsov/prelude

(use-package rainbow-mode)

(with-eval-after-load 'css-mode
  (prelude-require-packages '(rainbow-mode))

  (setq css-indent-offset 2)

  (defun prelude-css-mode-defaults ()
    (rainbow-mode +1)
    (run-hooks 'prelude-prog-mode-hook))

  (setq prelude-css-mode-hook 'prelude-css-mode-defaults)

  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'prelude-css-mode-hook))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
