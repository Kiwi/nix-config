;;; -*- lexical-binding: t; -*-

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
(with-eval-after-load 'magit
  (use-package fullframe :demand
    :config
    (fullframe magit-status magit-mode-quit-window)
    (fullframe projectile-vc magit-mode-quit-window)
    (fullframe magit-diff-staged magit-quit-window)
    (fullframe magit-diff-unstaged magit-mode-quit-window)
    (fullframe magit-diff magit-mode-quit-window))

  (use-package magit-gitflow :demand
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

;; project awareness
(use-package projectile
  :bind (("C-c p" . projectile-command-map))
  :config (projectile-mode t))

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

(use-package whitespace :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing)))

;; per project specific code style settings
(use-package editorconfig :init
  (add-hook 'after-init-hook 'editorconfig-mode))

;; automatically indent code
(use-package aggressive-indent :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

;; one search to rule them all. C-s / C-r
(use-package ace-isearch :demand
  :after avy
  :config
  (global-ace-isearch-mode +1)
  (custom-set-variables
   '(ace-isearch-input-length 7)
   '(ace-isearch-jump-delay 0.25)
   '(ace-isearch-function 'avy-goto-char)
   '(ace-isearch-use-jump 'printing-char))
  (define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch))

;; precise char jumping (used by ace-isearch)
(use-package avy :demand
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;; highlight search words in the context also (used by ace-isearch)
(use-package anzu :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  :config
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

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
  (add-hook 'after-init-hook 'company-quickhelp-mode))

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
  (with-eval-after-load "which-func"
    '(setq which-func-modes '(emacs-lisp-mode
                              common-lisp-mode
                              clojure-mode
                              cc-mode
                              js2-mode
                              css-mode
                              scss-mode
                              web-mode
                              org-mode))))

;; mimic popular IDEs binding, note that it doesn't work in a terminal session
(use-package crux :demand
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key (kbd "M-o") 'crux-smart-open-line)
  (global-set-key [(control shift return)] 'crux-smart-open-line-above)
  (global-set-key [(control shift up)]  'move-text-up)
  (global-set-key [(control shift down)]  'move-text-down)
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down)
  (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c f") 'crux-recentf-ido-find-file)
  (global-set-key (kbd "C-M-z") 'crux-indent-defun)
  (global-set-key (kbd "C-c u") 'crux-view-url)
  (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
  (global-set-key (kbd "C-c s") 'crux-swap-windows)
  (global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
  (global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
  (global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
  (global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
  (global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
  (global-set-key (kbd "C-c I") 'crux-find-user-init-file)
  (global-set-key (kbd "C-c S") 'crux-find-shell-init-file))

;; smartparens is excellent for making nuanced text edits and working with pairs
;; adapted from ;; https://raw.githubusercontent.com/Fuco1/.emacs.d/master/files/smartparens.el
(use-package smartparens :demand
  :config (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)

  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinding management
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)

  (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
  (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

  (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
  (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
  (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
  (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
  (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
  (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
  (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
  (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
  (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
  (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
  (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
  (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
  (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
  (defvar hyp-s-x-map)
  (define-prefix-command 'hyp-s-x-map)
  (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
  (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
  (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
  (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

  (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;;;;;;;;;;;;;;;;;
  ;; pair management

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

    ;;; lisp modes
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-("
                   :pre-handlers '(my-add-space-before-sexp-insertion)
                   :post-handlers '(my-add-space-after-sexp-insertion)))

  (defun my-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun my-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp))
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

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

(use-package hydra :demand
  :after smartparens
  :config
  ;; ESC and <insert> activate a modal layer with precision edit features...
  (define-key key-translation-map (kbd "ESC") (kbd "C-M-s"))
  (define-key key-translation-map (kbd "<insert>") (kbd "C-M-s"))
  (bind-key "C-M-s"
            (defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map))

;; a final place to put general programming things before lang-specific configs
(defun my-prog-mode-hook ()
  "Programming global stuff."
  (smartparens-mode +1) ;; ensures smartparens
  (goto-address-prog-mode 1)) ;; always make url clickable in prog-mode
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(provide 'ide)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
