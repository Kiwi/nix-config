;;; -*- lexical-binding: t; -*-

(define-key key-translation-map (kbd "s-g") (kbd "C-g"))

;; TODO FIXME make a new hydra swap to caps
(define-key key-translation-map (kbd "ESC") (kbd "<f4>"))

(global-set-key (kbd "<f1>") 'my/shell)
(global-set-key (kbd "<f2>") 'gnus)
(global-set-key (kbd "<f3>") 'my/erc-bounce)
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

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
