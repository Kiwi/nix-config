;;; -*- lexical-binding: t; -*-

;; Debugging options.
;; (setq debug-on-error t)
;; (setq debug-on-quit t)
;; (setq edebug-all-forms t)
;; (setq exwm-debug-on t)
;; (setq use-package-verbose t)
(toggle-frame-maximized)

;;; Set garbage collection temporarily to a large number, then back to default.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

;; Use an external custom.el file instead of appending customization variables to init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; allow access from emacsclient.
(server-start)

;; User information.
(setq user-full-name "Adam Schaefers"
      user-mail-address "sch@efers.org")

;; Startup settings.
(setq inhibit-startup-screen nil
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message nil)

;; Make Emacs verify tls certificates (depends: gnutls-cli certifi)
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

;; Bootstrap straight.el
(setq straight-recipes-gnu-elpa-use-mirror t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; `use-package' integration
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-always-defer t)

;; Begin installing packages using the `use-package' macro for now on.

;; Make some extra libs available for Emacs hackers
(require 'cl)
(require 'seq)
(use-package dash :demand)
(use-package ht :demand)
(use-package s :demand)
(use-package a :demand)

;; Begin lisp.d/ execution.

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/lisp.d")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
