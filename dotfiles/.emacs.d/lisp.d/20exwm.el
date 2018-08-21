;;; -*- lexical-binding: t; -*-


;; adapted from https://github.com/ch11ng/exwm/wiki/Configuration-Example

(use-package xelb :demand)
(use-package exwm :demand
  :config
  ;; Set the initial number of workspaces (they can also be created later).
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
		       (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
		      (interactive)
		      (start-process "" nil "/usr/bin/slock")))))

  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
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

  ;; disable simulation keys for e.g. Firefox
  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             (when (and exwm-class-name
  ;;                        (string= exwm-class-name "Firefox"))
  ;;               (exwm-input-set-local-simulation-keys nil))))

  (exwm-enable))

;; Make use of the Function Row.
(exwm-input-set-key (kbd "<f1>") 'helm-mini)
(exwm-input-set-key (kbd "<f2>") 'helm-run-external-command)
(exwm-input-set-key (kbd "<f3>") 'exwm-input-toggle-keyboard)
;; (exwm-input-set-key (kbd "<f4>") ')
;; (exwm-input-set-key (kbd "<f5>") ')
;; (exwm-input-set-key (kbd "<f6>") ')
;; (exwm-input-set-key (kbd "<f7>") ')
;; (exwm-input-set-key (kbd "<f8>") ')
(exwm-input-set-key (kbd "<f9>") 'kill-buffer-and-window)
(exwm-input-set-key (kbd "<f10>") 'delete-other-windows)
(exwm-input-set-key (kbd "<f11>") 'winner-undo)
(exwm-input-set-key (kbd "<f12>") 'winner-redo)

(use-package desktop-environment :demand
  :config
  (desktop-environment-mode)
  (setq desktop-environment-brightness-get-command "xbacklight -getf"
        desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
        desktop-environment-brightness-set-command "sudo xbacklight %s"
        desktop-environment-brightness-small-decrement "-dec 5"
        desktop-environment-brightness-small-increment "-inc 5"
        desktop-environment-brightness-normal-increment "-inc 20"
        desktop-environment-brightness-normal-decrement "-dec 10"))
(provide '20exwm)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
