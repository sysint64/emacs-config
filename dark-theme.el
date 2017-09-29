(load-theme 'wilson t)
(add-to-list 'load-path "~/.emacs.d/airline-themes")
(require 'airline-themes)
;; (load-theme 'airline-distinguished)
;; (load-theme 'airline-molokai)
;; (load-theme 'airline-powerlineish)
;; (load-theme 'airline-raven)
;; (load-theme 'airline-serene)
;; (load-theme 'airline-solarized-alternate-gui)
;; (load-theme 'airline-solarized-gui)
(load-theme 'airline-ubaryd)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(diff-hl-change ((t (:background "#94785e"))))
 ;; '(diff-hl-delete ((t (:background "#ae5151"))))
 ;; '(diff-hl-insert ((t (:background "#44924a"))))
 ;; '(show-paren-match ((t (:background "#345" :foreground "red" :weight normal))))
 ;; '(show-paren-mismatch ((t (:foreground "#555" :weight bold))))
 ;; '(sp-pair-overlay-face ((t (:background "#345"))))
 ;; '(sp-wrap-overlay-closing-pair ((t (:inherit sp-wrap-overlay-face :foreground "#345"))))
 ;; '(sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "forest green"))))
 '(window-divider ((t (:foreground "gray11"))))
 '(window-divider-first-pixel ((t nil)))
 '(window-divider-last-pixel ((t nil))))
;; (set-face-attribute 'region nil :background "#4f5b66")
;; (set-face-foreground 'font-lock-comment-delimiter-face "#65737e")

(setq window-divider-default-right-width 2)
(window-divider-mode)

;; (set-face-background 'show-paren-match "light blue")
;; (set-face-foreground 'show-paren-match "blue")
(set-face-attribute 'show-paren-match nil :weight 'normal)

(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

(set-face-attribute 'font-lock-constant-face nil :weight 'bold)
(set-face-foreground 'font-lock-constant-face "#9ba657")

(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face
      rainbow-identifiers-cie-l*a*b*-lightness 50
      rainbow-identifiers-cie-l*a*b*-saturation 20
      rainbow-identifiers-cie-l*a*b*-color-count 65536)

(provide 'dark-theme)
