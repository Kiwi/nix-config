;;; -*- lexical-binding: t; -*-

(use-package emms
  :after hydra
  :init
  (defhydra emms-hydra ()
    "Emacs Multimedia System"
    ("e" emms "emms")
    ("h" helm-emms "helm-emms")
    ("f" emms-play-file "play file")
    ("d" emms-play-directory "play directory")
    ("q" nil "Quit" :color blue))
  :bind (("<f4>" . emms-hydra/body))
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
