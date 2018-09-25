;;; -*- lexical-binding: t; -*-

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
  (my/shell))
(global-set-key (kbd "C-c K") 'my/kill-all-buffers)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
