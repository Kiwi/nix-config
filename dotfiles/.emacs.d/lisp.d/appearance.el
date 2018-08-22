;;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :disabled
  :config
  (load-theme 'doom-city-lights t))

(use-package gruvbox-theme
  :demand
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package rich-minority
  :init
  (add-hook 'after-init-hook 'rich-minority-mode)
  :config
  (setf rm-blacklist ""))

;; font and size
(set-face-attribute 'default nil :font "Source Code Pro 15")

;; set smaller size for modeline and minibuffer / echo area
(set-face-attribute 'mode-line nil  :height 0.75)
(set-face-attribute 'mode-line-inactive nil  :height 0.75)
;; smaller size in the minibuffer
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 0.75)))
  ;; smaller font size in the echo area.
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 0.75) variable-pitch)))))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

;; ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell nil)
(global-hl-line-mode 1)
(fringe-mode 9)
(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode 1)

(provide 'appearance)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
