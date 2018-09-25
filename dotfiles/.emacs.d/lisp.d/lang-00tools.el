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
(use-package diff-hl :init :disabled
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  :config
  ;; (diff-hl-flydiff-mode 1) https://github.com/dgutov/diff-hl/issues/65
  (define-key diff-hl-mode-map
    (kbd "<left-fringe> <mouse-1>")
    'diff-hl-diff-goto-hunk)
  (setq diff-hl-side 'right))

;; show the function your working with on the modeline
(require 'which-func)
(setq which-func-modes '(emacs-lisp-mode
                         common-lisp-mode
                         clojure-mode
                         cc-mode
                         js2-mode
                         css-mode
                         scss-mode
                         web-mode
                         org-mode))

(use-package hydra :demand)

(use-package crux :demand)

(use-package smartparens :demand
  :config (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  ;; these next two functions get used by some prelude lang configs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
		(crux-smart-open-line-above)) "RET"))))

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
