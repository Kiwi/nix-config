;;; -*- lexical-binding: t; -*-
(global-unset-key (kbd "C-z"))

(defun my/binds ()
  ;; general
  (define-key key-translation-map (kbd "s-g") (kbd "C-g"))
  (global-set-key (kbd "<f1>") 'my/shell)
  (global-set-key (kbd "<f2>") 'gnus)
  (global-set-key (kbd "<f3>") 'my/erc-bounce)
  (defhydra emms-hydra (:exit t)
    "Emacs Multimedia System"
    ("h" helm-emms "helm-emms")
    ("f" emms-play-file "play file")
    ("d" emms-play-directory "play directory")
    ("p" emms "playlist")
    ("q" nil "Quit" :color blue))
  (global-set-key (kbd "<f4>") 'emms-hydra/body)
  (global-set-key (kbd "<menu>") 'helm-mini)
  (global-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
  (global-set-key (kbd "s-<tab>") 'spacemacs/alternate-buffer)
  (global-set-key (kbd "") (lambda ()
                             (interactive)
                             (other-window -1)))
  (global-set-key (kbd "s-l") 'spacemacs/alternate-window)
  (global-set-key (kbd "s-/") 'winner-undo)
  (global-set-key (kbd "s-?") 'winner-redo)
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-below)
  (global-set-key (kbd "s-3") 'split-window-right)
  (global-set-key (kbd "s-0") 'delete-window)

  ;; primary text-editing hydra on CAPS via setxkbmap -option caps:swapescape
  (defhydra smartparens-hydra (:exit t)
    "Smartparens"
    ("d" sp-down-sexp "Down")
    ("e" sp-up-sexp "Up")
    ("u" sp-backward-up-sexp "Up")
    ("a" sp-backward-down-sexp "Down")
    ("f" sp-forward-sexp "Forward")
    ("b" sp-backward-sexp "Backward")
    ("k" sp-kill-sexp "Kill" :color blue)
    ("q" nil "Quit" :color blue))
  (define-key key-translation-map (kbd "ESC") (kbd "<f13>"))
  (global-set-key (kbd "<f13>") 'smartparens-hydra/body)

  ;; mimic popular IDEs binding
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
  (global-set-key (kbd "C-c S") 'crux-find-shell-init-file)


  ;;TODO work these into my system, remap and hydras !!!

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

  (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map))

(add-hook 'after-init-hook 'my/binds)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
