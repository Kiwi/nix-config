;;; -*- lexical-binding: t; -

;; use the nixos bash shell with M-x shell
(setq explicit-shell-file-name "/run/current-system/sw/bin/bash")

;; use real bash-completion with shell-mode
(use-package bash-completion
  :bind (("C-c as" . shell))
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
