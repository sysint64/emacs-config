(setq e2ml-highlights
      '(("\\b[A-Z]\\([a-zA-Z0-9_]+\\)?\\b" . font-lock-keyword-face)
        ("\\b[a-z]\\([a-zA-Z0-9_]+\\)?\\b" . font-lock-constant-face),
        ))

(define-derived-mode e2ml-mode python-mode "e2ml major mode"
  "major mode for editing e2ml language code."
  (setq font-lock-defaults '(e2ml-highlights)))

(add-to-list 'auto-mode-alist '("\\.e2t\\'" . e2ml-mode))
(add-to-list 'auto-mode-alist '("\\.e2ml\\'" . e2ml-mode))
