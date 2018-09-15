;;; -*- lexical-binding: t; -*-

(use-package xelb :demand)
(use-package exwm :demand
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)
  (exwm-enable)

  (setq exwm-workspace-number 1)

  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title)))))

(use-package desktop-environment :demand :after exwm
  :config
  (desktop-environment-mode)
  (setq desktop-environment-brightness-get-command "light -G"
        desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
        desktop-environment-brightness-set-command "light %s"
        desktop-environment-brightness-small-decrement "-A 5"
        desktop-environment-brightness-small-increment "-U 5"
        desktop-environment-brightness-normal-increment "-A 10"
        desktop-environment-brightness-normal-decrement "-U 10")
  (unbind-key "s-l" desktop-environment-mode-map)
  (exwm-input-set-key (kbd "s-l") nil)
  (exwm-input--update-global-prefix-keys))

(with-eval-after-load "desktop-environment"
  (exwm-input-set-key (kbd "s-s") 'my/shell)
  (exwm-input-set-key (kbd "<s-return>") 'my/shell)
  (exwm-input-set-key (kbd "s-a") 'my/ansi-term)
  (exwm-input-set-key (kbd "<H-return>") 'my/ansi-term)
  (exwm-input-set-key (kbd "s-g") 'gnus)
  (exwm-input-set-key (kbd "s-i") 'my/erc-bounce)
  (exwm-input-set-key (kbd "s-e") 'emms-hydra/body)
  (exwm-input-set-key (kbd "<f17>") 'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "<f18>") 'helm-mini)
  (exwm-input-set-key (kbd "<f19>") 'helm-run-external-command)

  ;; window mgmt
  (exwm-input-set-key (kbd "s-<tab>") 'spacemacs/alternate-buffer)
  (exwm-input-set-key (kbd "s-[") 'previous-buffer)
  (exwm-input-set-key (kbd "s-]") 'next-buffer)
  (exwm-input-set-key (kbd "s-o") (lambda ()
                                    (interactive)
                                    (other-window -1)))
  (exwm-input-set-key (kbd "s-l") 'spacemacs/alternate-window)
  (exwm-input-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
  (exwm-input-set-key (kbd "s-/") 'winner-undo)
  (exwm-input-set-key (kbd "s-?") 'winner-redo)
  (exwm-input-set-key (kbd "s-0") 'delete-window)
  (exwm-input-set-key (kbd "s-1") 'delete-other-windows)
  (exwm-input-set-key (kbd "s-2") 'split-window-below)
  (exwm-input-set-key (kbd "s-3") 'split-window-right)

  (exwm-input-set-key (kbd "s-m") 'my/next-monitor))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
