;;; -*- lexical-binding: t; -*-


;; adapted from https://github.com/ch11ng/exwm/wiki/Configuration-Example

;; currently testing packaged version from nix.
(require 'exwm)
(require 'exwm-config)

;; (use-package xelb :demand)
;; (use-package exwm :demand
;;   :config
;;   ;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 1)

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))))

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
              (exwm-workspace-rename-buffer exwm-title))))

(exwm-input-set-key (kbd "<f12>")
                    '(lambda ()
                       (interactive)
                       (start-process-shell-command "bash ~/bin/nextmonitor.sh" nil "bash ~/bin/nextmonitor.sh")))
(exwm-input-set-key (kbd "<f11>")
                    '(lambda ()
                       (interactive)
                       (start-process-shell-command "xmodmap ~/.Xmodmap ; xset r rate 250 50" nil "xmodmap ~/.Xmodmap ; xset r rate 250 50")))
;; Window and Buffer management
(exwm-input-set-key (kbd "<f1>") 'my/shell)
(exwm-input-set-key (kbd "<f2>") 'gnus)
(exwm-input-set-key (kbd "<f3>") 'my/erc-bounce)
(exwm-input-set-key (kbd "<f4>") 'emms-hydra/body)
(exwm-input-set-key (kbd "H-w") (lambda ()
                                  (interactive)
                                  (other-window -1)))
(exwm-input-set-key (kbd "H-<iso-lefttab>") 'spacemacs/alternate-window)
(exwm-input-set-key (kbd "H-<tab>") 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<f19>") 'helm-run-external-command)
(exwm-input-set-key (kbd "<f18>") 'helm-mini)
(exwm-input-set-key (kbd "<f17>") 'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "H-q") 'kill-buffer-and-window)
(exwm-input-set-key (kbd "H-d") 'delete-window)
(exwm-input-set-key (kbd "H-/") 'winner-undo)
(exwm-input-set-key (kbd "H-?") 'winner-redo)
(exwm-input-set-key (kbd "H-1") 'delete-other-windows)
(exwm-input-set-key (kbd "H-2") 'split-window-below)
(exwm-input-set-key (kbd "H-3") 'split-window-right)

;; external displays
(require 'exwm-randr)
(exwm-randr-enable)

(exwm-enable)
;; )

(use-package desktop-environment :demand
  :config
  (desktop-environment-mode)
  (setq desktop-environment-brightness-get-command "xbacklight -get"
        desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
        desktop-environment-brightness-set-command "sudo xbacklight %s"
        desktop-environment-brightness-small-decrement "-dec 5"
        desktop-environment-brightness-small-increment "-inc 5"
        desktop-environment-brightness-normal-increment "-inc 10"
        desktop-environment-brightness-normal-decrement "-dec 10"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
