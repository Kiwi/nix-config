;;; -*- lexical-binding: t; -*-

(defun my/compile-init ()
  "Auto byte compile everything if needed."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/lisp.d" 0)
  (byte-compile-file "~/.emacs.d/init.el"))
;; (unless (file-exists-p "~/.emacs.d/init.elc")
;;   (add-hook 'emacs-startup-hook 'my/compile-init))

(defun my/update-init ()
  "This is how I sync my configuration across machines."
  (interactive)
  (async-shell-command "~/.emacs.d/update-init.sh"))

(defun my/xprofile-reload ()
  (interactive)
  (start-process-shell-command "" nil "bash ~/.xprofile"))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

(global-set-key (kbd "C-x o") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "C-x w") 'spacemacs/alternate-window)
(global-set-key (kbd "C-x <tab>") 'spacemacs/alternate-buffer)

(defun my/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (my-startup-screen-hook))
(global-set-key (kbd "C-c K") 'my/kill-all-buffers)

;; nougat boilerplate functions

(defun global-kbd-or (&rest args)
  (let* ((pairs (-partition 2 args)))
    (cl-loop for pair in pairs
             for target = (car pair)
             for default = (cadr pair)
             for target-name = (symbol-name target)
             for override-name = (format "kbd-%s" target-name)
             for override-symbol = (make-symbol override-name)
             if (boundp override-symbol)
             do (global-set-key (kbd (eval override-symbol)) target)
             else
             do (global-set-key (kbd default) target))))

(defun define-kbd-or (&rest args)
  (let* ((triplets (-partition 3 args)))
    (cl-loop for triplet in triplets
             for mode = (nth 0 triplet)
             for target = (nth 1 triplet)
             for default = (nth 2 triplet)
             for target-name = (symbol-name target)
             for override-name = (format "kbd-%s" target-name)
             for override-symbol = (make-symbol override-name)
             if (boundp override-symbol)
             do (define-key mode (kbd (eval override-symbol)) target)
             else
             do (define-key mode (kbd default) target))))

(defun kbd-or (target default)
  (if (boundp target)
      (kbd (eval target))
    (kbd default)))

(defun var-or (target default)
  (if (boundp target)
      (eval target)
    default))

(provide '10myfunc)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
