;;; -*- lexical-binding: t; -*-

;; mostly misc. settings grabbed from technomancy's emacs-starter-kit,
;; Prelude starter-kit, and emacswiki all mixed up with my own preferences.

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(prefer-coding-system 'utf-8)

(setq select-enable-clipboard t
 select-enable-primary nil
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t)

(delete-selection-mode 1)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defalias 'yes-or-no-p 'y-or-n-p)

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

;; Auto-indent current and new lines
(electric-indent-mode -1) ; using aggressive-indent (in 40ide.el) instead

;; Auto-indent yanked (pasted) code (this is still nice to have with aggressive-indent)
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; default browsers
(setq browse-url-browser-function 'browse-url-chromium)

;; ediff winner-undo hook afterwards to resume layout.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
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

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
