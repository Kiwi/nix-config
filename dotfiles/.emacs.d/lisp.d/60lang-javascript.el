;;; -*- lexical-binding: t; -*-


(use-package json-mode)
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  :config
  (defun prelude-js-mode-defaults ()
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1))

  (add-hook 'js2-mode-hook 'prelude-js-mode-defaults))

(provide '60lang-js)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
