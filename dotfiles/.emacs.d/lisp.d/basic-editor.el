;;; -*- lexical-binding: t; -*-

(use-package doom-themes :demand
  :config (load-theme 'doom-one t))

(use-package smart-mode-line :demand
  :init (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  :config (sml/setup))

(use-package rich-minority
  :config (setf rm-blacklist ""))

;; set "emacsclient" frame font and size
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15" ))
;; set "emacs" font and size
(set-face-attribute 'default nil :font "Source Code Pro 15")

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

;; needed for ratpoison
(setq frame-resize-pixelwise t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq
 x-select-enable-clipboard-manager nil
 select-enable-clipboard t
 select-enable-primary nil
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 vc-follow-symlinks t
 ediff-window-setup-function 'ediff-setup-windows-plain
 save-place-file (concat user-emacs-directory "places")
 backup-directory-alist `(("." . ,(concat user-emacs-directory
                                          "backups"))))

(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq tab-always-indent 'complete)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indent code
(use-package aggressive-indent :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; ;; Auto-indent current and new lines
;; (electric-indent-mode 1)

;; ;; Auto-indent yanked (pasted) code
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (member major-mode '(emacs-lisp-mode lisp-mode
;;                                                      clojure-mode    scheme-mode
;;                                                      haskell-mode    ruby-mode
;;                                                      rspec-mode      python-mode
;;                                                      c-mode          c++-mode
;;                                                      objc-mode       latex-mode
;;                                                      plain-tex-mode))
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))

(require 'whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-style '(face empty tabs lines-tail trailing))

;; per project code style settings
(use-package editorconfig :init
  (add-hook 'prog-mode-hook 'editorconfig-mode))

(add-hook 'text-mode-hook 'abbrev-mode)

;; default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; ediff winner-undo hook afterwards to resume layout.
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; load all the dired features also use dired-async-mode for performance.
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(use-package async :demand
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

;; clean up obsolete buffers every 2 hours.
(use-package midnight :demand
  :config
  (setq midnight-period 7200))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(use-package expand-region :demand)

(require 'winner)
(winner-mode 1)

;; minibuffer, stop cursor going into it
(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; Tramp gives me problems opening large text files without this.
(setq tramp-copy-size-limit nil)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
