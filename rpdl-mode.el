(setq rpdl-highlights
      '(("\\b[A-Z]\\([a-zA-Z0-9_]+\\)?\\b" . font-lock-keyword-face)
        ("\\b[a-z]\\([a-zA-Z0-9_]+\\)?\\b" . font-lock-constant-face),
        ))

(define-derived-mode rpdl-mode python-mode "rpdl major mode"
  "major mode for editing rpdl language code."
  (setq font-lock-defaults '(e2ml-highlights)))

(add-to-list 'auto-mode-alist '("\\.rdl\\'" . rpdl-mode))
(add-to-list 'auto-mode-alist '("\\.rpdl\\'" . rpdl-mode))
