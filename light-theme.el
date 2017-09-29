(load-theme 'brin t)
(load-theme 'mccarthy t)
(load-theme 'airline-doom-one)
(set-face-foreground 'font-lock-string-face "SpringGreen4")
(set-face-foreground 'font-lock-comment-face "dark gray")
(set-face-foreground 'font-lock-comment-delimiter-face "dark gray")

;; Highlight paren
(set-face-background 'show-paren-match "light blue")
(set-face-foreground 'show-paren-match "blue")
(set-face-attribute 'show-paren-match nil :weight 'normal)

(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

(set-face-foreground 'font-lock-constant-face "#DA3C01")
(set-face-attribute 'font-lock-constant-face nil :weight 'bold)
(set-face-foreground 'font-lock-function-name-face "#DA3C01")

(set-face-foreground 'font-lock-constant-face "#3b5998")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(setq window-divider-default-right-width 2)
(window-divider-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "wheat"))))
 '(diff-hl-delete ((t (:background "RosyBrown1"))))
 '(diff-hl-insert ((t (:background "DarkSeaGreen2"))))
 '(show-paren-match ((t (:background "wheat" :foreground "red" :weight normal))))
 '(show-paren-mismatch ((t (:foreground "#555" :weight bold))))
 '(sp-pair-overlay-face ((t (:background "wheat"))))
 '(sp-wrap-overlay-closing-pair ((t (:inherit sp-wrap-overlay-face :foreground "firebrick"))))
 '(sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "forest green"))))
 '(window-divider ((t (:foreground "gray18"))))
 '(window-divider-first-pixel ((t nil)))
 '(window-divider-last-pixel ((t nil))))
(set-face-attribute 'region nil :background "wheat")

(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face
      rainbow-identifiers-cie-l*a*b*-lightness 50
      rainbow-identifiers-cie-l*a*b*-saturation 50
      rainbow-identifiers-cie-l*a*b*-color-count 65536)

(provide 'light-theme)
