
;;; Code:

(add-to-list 'load-path "~/.emacs.d/doom-themes")
(require 'doom-themes)
;; (load-theme 'airline-doom-one)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-one t)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#5B6268"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65")))))

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(set-fringe-mode '(5 . 0))
(set-face-attribute 'fringe nil :background "#21242b")

(require 'solaire-mode)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (turn-on-solaire-mode)
            (set-face-attribute 'fringe nil :background "#21242b")
            ))

(add-hook 'after-revert-hook
          (lambda ()
            (turn-on-solaire-mode)
            (set-face-attribute 'fringe nil :background "#21242b")
            ))

(provide 'doom-theme)
