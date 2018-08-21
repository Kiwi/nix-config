;;; -*- lexical-binding: t; -*-

(use-package emms
  :bind (("C-c aee" . emms))
  :bind (("C-c aef" . emms-play-file))
  :bind (("C-c aed" . emms-play-directory))
  :config
  (setq emms-source-file-default-directory "/mnt/ZSTORE/")
  (emms-all)
  (emms-default-players))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t))

(provide '80multimedia)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
