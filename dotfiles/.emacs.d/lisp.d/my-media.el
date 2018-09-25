;;; -*- lexical-binding: t; -*-

(use-package emms
  :config
  (setq emms-source-file-default-directory "~/Downloads")
  (emms-all)
  (emms-default-players))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
