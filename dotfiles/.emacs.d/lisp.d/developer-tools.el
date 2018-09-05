;; -*- lexical-binding: t; -*-

;; project awareness
(use-package projectile
  :after helm
  :bind (("C-c p" . projectile-command-map))
  :config (projectile-mode t)
  (setq projectile-completion-system 'helm))

;; version control
(use-package magit
  :bind (("C-c gs" . magit-status)
         ("C-c gl" . magit-log)
         ("C-c gf" . magit-log-buffer-file)
         ("C-c gb" . magit-blame))
  :config
  (setq-default magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/repos" . 1)
                                       ("~/.emacs.d/straight/repos" . 1)
                                       ("/nix-config" . 0))))

(use-package fullframe :demand :after magit
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe projectile-vc magit-mode-quit-window)
  (fullframe magit-diff-staged magit-quit-window)
  (fullframe magit-diff-unstaged magit-mode-quit-window)
  (fullframe magit-diff magit-mode-quit-window))

(use-package magit-gitflow :demand :after magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; snippets
(use-package yasnippet-snippets)
(use-package yasnippet
  :bind (("C-x C-<tab>" . yas-insert-snippet))
  :config (yas-global-mode 1))

;; jump to definitions
(use-package dumb-jump
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look))
  :config (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm))

;; one search to rule them all. C-s / C-r
(use-package ace-isearch :demand
  :after avy
  :config
  (global-ace-isearch-mode +1)
  (custom-set-variables
   '(ace-isearch-input-length 7)
   '(ace-isearch-jump-delay 0.50)
   '(ace-isearch-function 'avy-goto-char)
   '(ace-isearch-use-jump 'printing-char))
  (define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch))

;; precise char jumping (used by ace-isearch)
(use-package avy :demand
  :after helm-swoop
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;; discoverable binds
(use-package which-key :init
  (add-hook 'after-init-hook 'which-key-mode)
  :config
  (setq which-key-idle-delay 0.0))

;; auto-completion
(use-package company :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0))
(use-package company-quickhelp :init
  (add-hook 'after-init-hook 'company-quickhelp-mode)
  :config (setq company-quickhelp-delay nil)
  (setq company-global-modes '(not shell-mode))
  (define-key company-active-map (kbd "<f1>") #'company-quickhelp-manual-begin))

;; linting in the fringe
(use-package flycheck :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; highlight the keywords: TODO, FIXME, and NOTE
(use-package hl-todo :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;; git diff's in the fringe/ modeline
(use-package diff-hl :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  :config
  ;; (diff-hl-flydiff-mode 1) https://github.com/dgutov/diff-hl/issues/65
  (define-key diff-hl-mode-map
    (kbd "<left-fringe> <mouse-1>")
    'diff-hl-diff-goto-hunk)
  (setq diff-hl-side 'right))

;; show the function your working with on the modeline
(use-package which-func :init
  (add-hook 'after-init-hook 'which-function-mode)
  :config
  (with-eval-after-load 'which-func
    (setq which-func-modes '(emacs-lisp-mode
                             common-lisp-mode
                             clojure-mode
                             cc-mode
                             js2-mode
                             css-mode
                             scss-mode
                             web-mode
                             org-mode))))

;; a final place to put general programming things
(defun my-prog-mode-hook ()
  "Programming global stuff."
  (smartparens-mode +1) ;; ensures smartparens
  (goto-address-prog-mode 1)) ;; always make url clickable in prog-mode
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
