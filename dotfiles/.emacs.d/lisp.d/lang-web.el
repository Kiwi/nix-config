;;; -*- lexical-binding: t; -*-

;; Adapted to my config from "Prelude". :)
;; Please check them out and give them github stars. https://github.com/bbatsov/prelude

(use-package markdown-mode)
(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

(sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-tag "%" "<% "  " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

(with-eval-after-load 'web-mode
  (defun prelude-web-mode-defaults ())
  (setq prelude-web-mode-hook 'prelude-web-mode-defaults)

  (add-hook 'web-mode-hook (lambda ()
                             (run-hooks 'prelude-web-mode-hook))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
