;;; -*- lexical-binding: t; -*-

(use-package xelb :demand)
(use-package exwm :demand
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)
  (exwm-enable)

  (setq exwm-workspace-number 1)
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
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

  (exwm-input-set-key (kbd "S-<f12>")
                      '(lambda ()
                         (interactive)
                         (start-process-shell-command "bash ~/bin/nextmonitor.sh" nil "bash ~/bin/nextmonitor.sh")))
  (exwm-input-set-key (kbd "S-<f11>")
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

  (exwm-input-set-key (kbd "H-<backspace>") 'kill-buffer-and-window)
  (exwm-input-set-key (kbd "H-/") 'winner-undo)
  (exwm-input-set-key (kbd "H-?") 'winner-redo)
  (exwm-input-set-key (kbd "H-0") 'delete-window)
  (exwm-input-set-key (kbd "H-1") 'delete-other-windows)
  (exwm-input-set-key (kbd "H-2") 'split-window-below)
  (exwm-input-set-key (kbd "H-3") 'split-window-right)

  ;; switch display to next monitor toggle.
  (exwm-input-set-key
   (kbd "s-m")
   '(lambda ()
      (interactive)
      (start-process-shell-command "bash ~/bin/nextmonitor.sh" nil "bash ~/bin/nextmonitor.sh")))

  ;; for when i unplug / replug keyboards or return from laptop suspend...
  (exwm-input-set-key
   (kbd "s-k")
   '(lambda ()
      (interactive)
      (start-process-shell-command "xmodmap ~/.Xmodmap" nil "xmodmap ~/.Xmodmap")
      (start-process-shell-command "xset r rate 250 50" nil "xset r rate 250 50")))

  ;; Window and Buffer management
  (exwm-input-set-key (kbd "<f1>") 'my/shell)
  (exwm-input-set-key (kbd "<f2>") 'gnus)
  (exwm-input-set-key (kbd "<f3>") 'my/erc-bounce)
  (exwm-input-set-key (kbd "<f4>") 'emms-hydra/body)

  (exwm-input-set-key (kbd "<f19>") 'helm-run-external-command) ;; xmodmap r-ctrl
  (exwm-input-set-key (kbd "<f18>") 'helm-mini) ;; xmodmap "menu"
  (exwm-input-set-key (kbd "<f17>") 'exwm-input-toggle-keyboard) ;; xmodmap l-ctrl

  (exwm-input-set-key (kbd "H-w") (lambda ()
                                    (interactive)
                                    (other-window -1)))
  (exwm-input-set-key (kbd "H-<iso-lefttab>") 'spacemacs/alternate-window)
  (exwm-input-set-key (kbd "H-<tab>") 'spacemacs/alternate-buffer)

  (exwm-input-set-key (kbd "H-q") 'kill-buffer-and-window)
  (exwm-input-set-key (kbd "H-d") 'delete-window)
  (exwm-input-set-key (kbd "H-/") 'winner-undo)
  (exwm-input-set-key (kbd "H-?") 'winner-redo)
  (exwm-input-set-key (kbd "H-1") 'delete-other-windows)
  (exwm-input-set-key (kbd "H-2") 'split-window-below)
  (exwm-input-set-key (kbd "H-3") 'split-window-right))

(use-package desktop-environment :demand
  :config
  (desktop-environment-mode)
  (setq desktop-environment-brightness-get-command "xbacklight -getf"
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
