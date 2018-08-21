;;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix?\\'" . nix-mode))
  (add-hook 'before-save-hook #'nix-mode-format)
  :config

  (defun nix-mode-make-regexp (parts)
    (declare (indent defun))
    (string-join parts "\\|"))

  (defun nix-mode-search-backward ()
    (re-search-backward nix-mode-combined-regexp nil t))

  (setq nix-mode-caps '(" =[ \n]" "\(" "\{" "\\[" "\\bwith " "\\blet\\b"))
  (setq nix-mode-ends '(";" "\)" "\\]" "\}" "\\bin\\b"))
  (setq nix-mode-caps-regexp (nix-mode-make-regexp nix-mode-caps))
  (setq nix-mode-ends-regexp (nix-mode-make-regexp nix-mode-ends))
  (setq nix-mode-combined-regexp (nix-mode-make-regexp (append nix-mode-caps nix-mode-ends)))

  (defun fixed-nix-indent-expression-start ()
    (let* ((ends 0)
           (once nil)
           (done nil)
           (indent (current-indentation)))
      (save-excursion
        ;; we want to indent this line, so we don't care what it contains
        ;; skip to the beginning so reverse searching doesn't find any matches within
        (beginning-of-line)
        ;; search backward until an unbalanced cap is found or no cap or end is found
        (while (and (not done) (nix-mode-search-backward))
          (cond ((looking-at nix-mode-ends-regexp)
                 ;; count the matched end
                 ;; this means we expect to find at least one more cap
                 (setq ends (+ ends 1)))
                ((looking-at nix-mode-caps-regexp)
                 ;; we found at least one cap
                 ;; this means our function will return true
                 ;; this signals to the caller we handled the indentation
                 (setq once t)
                 (if (> ends 0)
                     ;; this cap corresponds to a previously matched end
                     ;; reduce the number of unbalanced ends
                     (setq ends (- ends 1))
                   ;; no unbalanced ends correspond to this cap
                   ;; this means we have found the expression that contains our line
                   ;; we want to indent relative to this line
                   (setq indent (current-indentation))
                   ;; signal that the search loop should exit
                   (setq done t))))))
      ;; done is t when we found an unbalanced expression cap
      (when done
        ;; indent relative to the indentation of the expression containing our line
        (indent-line-to (+ tab-width indent)))
      ;; return t to the caller if we found at least one cap
      ;; this signals that we handled the indentation
      once))

  (defun nix-mode-format ()
    "Format the entire nix-mode buffer"
    (interactive)
    (when (eq major-mode 'nix-mode)
      (save-excursion
        (beginning-of-buffer)
        (while (not (equal (point) (point-max)))
          (if (equal (string-match-p "^[\s-]*$" (thing-at-point 'line)) 0)
              (delete-horizontal-space)
            (nix-indent-line))
          (next-line)))))

  (define-key nix-mode-map (kbd "TAB") 'nix-indent-line)
  (setq nix-indent-function 'nix-indent-line)
  (defalias
    'nix-indent-expression-start
    'fixed-nix-indent-expression-start))

;; TODO https://github.com/shlevy/nix-buffer
;; https://github.com/travisbhartwell/nix-emacs

(provide 'nix)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
