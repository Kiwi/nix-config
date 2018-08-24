;;; -*- lexical-binding: t; -*-

(use-package doom-themes :demand
  :config (load-theme 'doom-city-lights t))

(use-package smart-mode-line :demand
  :init (setq sml/theme 'respectful)
  :config (sml/setup))

(use-package rich-minority
  :config (setf rm-blacklist ""))

;; set font and size
(set-face-attribute 'default nil :font "Source Code Pro 16")

;; set smaller font size for modeline
(set-face-attribute 'mode-line nil  :height 0.75)
(set-face-attribute 'mode-line-inactive nil  :height 0.75)

;; set smaller font size for minibuffer / echo area
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 0.75)))
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 0.75) variable-pitch)))))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

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
