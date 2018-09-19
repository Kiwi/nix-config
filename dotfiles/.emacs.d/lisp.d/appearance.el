;;; -*- lexical-binding: t; -*-

(use-package doom-themes :demand
  :config (load-theme 'doom-one t))

(use-package smart-mode-line :demand
  :init (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  :config (sml/setup))

(use-package rich-minority
  :config (setf rm-blacklist ""))

;; set font and size
(set-face-attribute 'default nil :font "Source Code Pro 15")

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(50 . 50))
(add-to-list 'default-frame-alist '(alpha . (50 . 50)))

;; show clock on modeline
(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode 1)

;; fringe used by flycheck and diff-hl
(fringe-mode 9)

;; highlight active line
(global-hl-line-mode 1)

;; cleanup the ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell nil)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
