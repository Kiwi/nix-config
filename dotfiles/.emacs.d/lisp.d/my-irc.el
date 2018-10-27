;;; -*- lexical-binding: t; -*-

(use-package erc :init
  (defun my/erc-bounce ()
    "Connect to an irc server using creds in .authinfo or .authinfo.gpg."
    (interactive)
    (setq erc-autojoin-timing "ident"
          erc-prompt-for-password nil
          erc-nick "adamantium"
          erc-autojoin-channels-alist '(("freenode.net"
                                         "#emacs"
                                         "#nixos"
                                         "#cooslug"
                                         "##apoptosis")))
    (erc-tls :server "chat.freenode.net" :port "6697"))
  :config

  ;; hide join/part/quit messages
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; friends list
  (setq erc-pals '("bayprogrammer"))

  ;; show only mentions and pals on my modeline
  (setq erc-format-query-as-channel-p t
        erc-track-priority-faces-only 'all
        erc-track-faces-priority-list '(erc-error-face
                                        erc-current-nick-face
                                        erc-keyword-face
                                        erc-nick-msg-face
                                        erc-direct-msg-face
                                        erc-dangerous-host-face
                                        erc-notice-face
                                        erc-prompt-face
                                        erc-pal-face))
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-track-exclude '("140.82.47.107:6698"
                            "chat.freenode.net:6697"
                            "freenode.net"))

  ;; "bury" private message buffers, but notify also on the modeline.
  (setq erc-auto-query 'bury)
  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value (intern "erc-current-nick-face"))
      ad-do-it))
  (defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
    (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
    ad-do-it
    (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
