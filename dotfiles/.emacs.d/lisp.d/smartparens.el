;; -*- lexical-binding: t; -*-

(use-package hydra :demand)

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

;; F16 is on caps lock !
(define-key key-translation-map (kbd "<f16>") (kbd "C-M-s"))
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
          smartparens-mode-map)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
