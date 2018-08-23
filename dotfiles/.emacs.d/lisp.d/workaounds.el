;;; -*- lexical-binding: t; -*-

;; minibuffer, stop cursor going into it http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; it still happens sometimes, but oh well
(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; Tramp gives me problems opening large text files without this.
(setq tramp-copy-size-limit nil)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
