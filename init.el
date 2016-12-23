(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d")

(setq show-paren-style 'parenthesis)
(show-paren-mode 1)
;; (toggle-truncate-lines)

(require 'paren)
(require 'highlight-parentheses)

(global-highlight-parentheses-mode)

(package-install 'php-mode)
(package-install 'magit)
(package-install 'diff-hl)

(global-diff-hl-mode)

(require 'neotree)
(global-set-key (kbd "M-1") 'neotree-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("6db9acac88c82f69296751e6c6d808736d6ff251dcb34a1381be86efc14fef54" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333" "da538070dddb68d64ef6743271a26efd47fbc17b52cc6526d932b9793f92b718" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "cf284fac2a56d242ace50b6d2c438fcc6b4090137f1631e32bedf19495124600" "228c0559991fb3af427a6fa4f3a3ad51f905e20f481c697c6ca978c5683ebf43" "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237" "d6db7498e2615025c419364764d5e9b09438dfe25b044b44e1f336501acd4f5b" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" default)))
 '(diff-hl-draw-borders nil)
 '(diff-hl-flydiff-mode t)
 '(helm-ff-auto-update-initial-value nil)
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("brown2")))
 '(neo-cwd-line-style (quote text))
 '(neo-fit-to-contents t)
 '(neo-smart-open t)
 '(neo-theme (quote nerd))
 '(neo-vc-integration (quote (face)))
 '(neo-window-position (quote left))
 '(package-selected-packages
   (quote
    (racer rust-mode alchemist smartparens elixir-mode helm-ag ack helm-projectile helm neotree buffer-move web-mode git-gutter nginx-mode diff-hl magit php-mode git-modes highlight-parentheses highlight-parentheses-mode install-package rainbow-identifiers highlight-numbers highlight-symbol multiple-cursors undo-tree redo-mode lua-mode d-mode flycheck-dmd-dub flycheck ac-dcd)))
 '(protect-buffer-bury-p nil)
 '(show-paren-mode t)
 '(window-divider-default-bottom-width 2)
 '(window-divider-default-right-width 2)
 '(window-divider-mode t))

(require 'magit)
(require 'diff-hl)

(global-set-key (kbd "M-9") 'magit-log)
(global-set-key (kbd "M-k") 'magit-status)
(global-set-key (kbd "C-k") 'magit-commit)
(global-set-key (kbd "C-S-k") 'magit-push)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;; (require 'autopair)
;; (defvar autopair-modes '(r-mode ruby-mode c-mode c++-mode python-mode elisp-mode java-mode d-mode))
;; (defun turn-on-autopair-mode () (autopair-mode 1))
;; (dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "<home>") 'smarter-move-beginning-of-line)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)

(require 'linum+)
(global-linum-mode 1)

(add-hook 'shell-mode (lambda () (linum-mode -1)))

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching 1)

(scroll-bar-mode -1)

(require 'bs)

(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(setq ac-auto-show-menu nil)
(ac-set-trigger-key "C-SPC")
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)

(define-key ac-completing-map (kbd "C-<down>") 'ac-next)
(define-key ac-completing-map (kbd "C-<up>") 'ac-previous)

(require 'popwin)

(require 'sr-speedbar)
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(speedbar-add-supported-extension ".d")

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes")

(add-to-list 'load-path "~/.emacs.d/projectile")
(require 'projectile)
(projectile-mode)

(global-set-key (kbd "<f3>") 'visit-tags-table)
(global-set-key (kbd "S-b") 'bookmark-set)
(global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

(defvar hs-special-modes-alist
  (mapcar 'purecopy
	  '((c-mode123321
	     "{" "}" "/[*/]" nil nil)
	    (c++-mode "{" "}" "/[*/]" nil nil)
	    (d-mode "{" "}" "/[*/]" nil nil)
	    (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
	    (java-mode "{" "}" "/[*/]" nil nil)
	    (js-mode "{" "}" "/[*/]" nil))))

(require 'hideshow)
(require 'helm)
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-to-list 'projectile-globally-ignored-directories ".dub")

(global-set-key (kbd "<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)
(global-set-key (kbd "C-S-n") 'helm-projectile)
(global-set-key (kbd "C-n") 'helm-projectile-grep)

(global-set-key (kbd "<XF86Calculator>") 'quick-calc)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "C-S-c <left>")  'buf-move-left)
(global-set-key (kbd "C-S-c <right>") 'buf-move-right)
(global-set-key (kbd "C-S-c <up>")    'buf-move-up)
(global-set-key (kbd "C-S-c <down>")  'buf-move-down)

(global-set-key (kbd "S-<f10>") 'projectile-compile-project)

(require 'bookmark)
(setq bookmark-save-flag t)
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
(global-set-key (kbd "<f3>") 'bookmark-set)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f5>") 'bookmark-bmenu-list)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

(defun my-enlarge-vert ()
  (interactive)
  (enlarge-window 2))

(defun my-shrink-vert ()
  (interactive)
  (enlarge-window -2))

(defun my-enlarge-horz ()
  (interactive)
  (enlarge-window-horizontally 2))

(defun my-shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))

(global-set-key (kbd "C-(") 'my-shrink-vert)
(global-set-key (kbd "C-)") 'my-enlarge-vert)
(global-set-key (kbd "M-C-(") 'my-shrink-horz)
(global-set-key (kbd "M-C-)") 'my-enlarge-horz)
(package-install 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-l") 'mc/edit-lines)
(global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-S-j") 'mc/mark-all-like-this)
(global-set-key (kbd "M-j") 'mc/mark-next-like-this-word)

(package-install 'highlight-symbol)
(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(package-install 'highlight-numbers)
(require 'highlight-numbers)

(add-hook 'd-mode-hook 'highlight-numbers-mode)

(package-install 'rainbow-identifiers)
(require 'rainbow-identifiers)

(require 'thingatpt)
(require 'thingatpt+)

(defun ash-forward-string (&optional arg)
  "Move forward to ARGth string."
  (setq arg (or arg 1))
  (if (not (bobp))
      (save-match-data
	(when (or (and (looking-at-p "\\s-*\"")
		       (not (looking-back "\\\\")))
		  (re-search-backward "[^\\\\]\"" nil nil))
	  (looking-at "\\s-*\"")
	  (goto-char (match-end 0))
	  (forward-char -1))))
  (while (and (> arg 0)
	      (not (eobp))
	      (looking-at-p "\\s-*\""))
    (forward-sexp 1)
    (setq arg (1- arg)))
  (while (and (< arg 0)
	      (not (bobp))
	      (looking-at-p "\""))
    (forward-sexp -1)
    (setq arg (1+ arg)))
  (ignore))

(put 'string 'forward-op 'ash-forward-string)

(defun select-symbol-under-cursor ()
  (interactive)
  (setq bounds (bounds-of-thing-at-point 'symbol))
  (set-mark (car bounds))
  (goto-char (cdr bounds))
  )

(defun select-sentence-under-cursor ()
  (interactive)
  (setq bounds (bounds-of-thing-at-point 'sentence))
  (set-mark (car bounds))
  (goto-char (cdr bounds)))

(defun select-string-under-cursor ()
  (interactive)
  (setq bounds (bounds-of-thing-at-point 'string))
  (set-mark (car bounds))
  (goto-char (cdr bounds)))

(global-set-key (kbd "C-d") 'select-symbol-under-cursor)
(global-set-key (kbd "C-S-d") 'select-sentence-under-cursor)
(global-set-key (kbd "C-S-s") 'select-string-under-cursor)

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;;

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

;; bind them to emacs's default shortcut keys:
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

; Lua
(package-install 'lua-mode)
(require 'lua-mode)

; VCS


; D Mode
(package-install 'd-mode)
(package-install 'flycheck)
(package-install 'flycheck-dmd-dub)

(require 'compile)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)@\\([^ \n]+\\):\\([0-9]+\\):\\([^\n]+\\)"
   2 3 nil (4 . 5)))

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\):\\([0-9]+\\) \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

;; Elixir Mode

(require 'elixir-mode)
(require 'alchemist)
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

(add-hook 'elixir-mode-hook
          (lambda ()
            (define-key elixir-mode-map (kbd "C-c ?") 'alchemist-help)
	    (define-key elixir-mode-map (kbd "C-c .") 'alchemist-goto-definition-at-point)
	    (define-key elixir-mode-map (kbd "C-c ,") 'alchemist-goto-jump-back)
            ))

(require 'smartparens)
(smartparens-global-mode t)
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

; Undo / Redo
(package-install 'undo-tree)
(require 'undo-tree)

(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-undo-tree-mode)

; Comment / Uncomment
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region-or-line)
(setq-default indent-tabs-mode nil)

(c-set-offset 'case-label '+)

(require 'd-mode)
(global-flycheck-mode t)
;(add-to-list 'load-path "~/.emacs.d/flycheck-dmd-dub")
;(require 'flycheck-dmd-dub)

(setq-default c-basic-offset 4)

;;; ac-dcd
(add-to-list 'load-path "~/.emacs.d/ac-dcd")
(require 'ac-dcd)

;;; ac-dcd
;; (require 'ac-dcd)

;; Rust

(require 'rust-mode)
;; (require 'compile)

;; (defun lcl:rust-compile-hook ()
;;   (set (make-local-variable 'compile-command)
;;        (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
;;            "cargo run"
;;          (format "rustc %s && %s" (buffer-file-name)
;;                  (file-name-sans-extension (buffer-file-name))))))

;; (setq-default compilation-read-command nil)
;; (add-hook 'rust-mode-hook 'lcl:rust-compile-hook)

;; (require 'racer)
;; (setq racer-rust-src-path "/home/andrey/dev/rustc-nightly/src/")

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; (add-hook 'rust-mode-hook
;; 	  (lambda ()
;;             (define-key rust-mode-map (kbd "C-c .") 'racer-find-definition)
;;             (define-key rust-mode-map (kbd "S-<f10>") 'compile)
;;             ))

;;

(add-hook 'd-mode-hook
	  (lambda ()
	    (auto-complete-mode t)
	    (setq-default indent-tabs-mode nil)
	    (when (featurep 'yasnippet) (yas-minor-mode-on))
	    (ac-dcd-maybe-start-server)
	    (ac-dcd-add-imports)
            (setq ac-auto-show-menu nil)
            (ac-set-trigger-key "C-SPC")

            (setq truncate-lines t)
            (define-key ac-completing-map [down] nil)
            (define-key ac-completing-map [up] nil)

            (define-key ac-completing-map (kbd "C-<down>") 'ac-next)
            (define-key ac-completing-map (kbd "C-<up>") 'ac-previous)

	    (add-to-list 'ac-sources 'ac-source-dcd)
	    (define-key d-mode-map (kbd "C-c ?") 'ac-dcd-show-ddoc-with-buffer)
	    (define-key d-mode-map (kbd "C-c .") 'ac-dcd-goto-definition)
	    (define-key d-mode-map (kbd "C-c ,") 'ac-dcd-goto-def-pop-marker)
	    (define-key d-mode-map (kbd "C-c s") 'ac-dcd-search-symbol)
	    (define-key d-mode-map (kbd "C-d") 'select-symbol-under-cursor)
	    (define-key d-mode-map (kbd "C-S-d") 'select-sentence-under-cursor)

	    (when (featurep 'popwin)
	      (add-to-list 'popwin:special-display-config
			   `(,ac-dcd-error-buffer-name :noselect t))
	      (add-to-list 'popwin:special-display-config
			   `(,ac-dcd-document-buffer-name :position right :width 80))
	      (add-to-list 'popwin:special-display-config
			   `(,ac-dcd-search-symbol-buffer-name :position bottom :width 5)))))

; (add-hook 'd-mode-hook 'ac-dcd-setup)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
;; (add-hook 'd-mode-hook          'hs-minor-mode)
(add-hook 'before-save-hook     'delete-trailing-whitespace)

(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(setq powerline-arrow-shape 'half)
(powerline-default-theme)

;; Dark theme
(load-theme 'brin t)
;; (add-to-list 'load-path "~/.emacs.d/airline-themes")
;; (require 'airline-themes)
;; (load-theme 'airline-murmur)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(diff-hl-change ((t (:background "#94785e"))))
;;  '(diff-hl-delete ((t (:background "#ae5151"))))
;;  '(diff-hl-insert ((t (:background "#44924a"))))
;;  '(show-paren-match ((t (:background "#345" :foreground "red" :weight normal))))
;;  '(show-paren-mismatch ((t (:foreground "#555" :weight bold))))
;;  '(sp-pair-overlay-face ((t (:background "#345"))))
;;  '(sp-wrap-overlay-closing-pair ((t (:inherit sp-wrap-overlay-face :foreground "#345"))))
;;  '(sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "forest green"))))
;;  '(window-divider ((t (:foreground "gray11"))))
;;  '(window-divider-first-pixel ((t nil)))
;;  '(window-divider-last-pixel ((t nil))))
;; (set-face-attribute 'region nil :background "#4f5b66")
;; (set-face-foreground 'font-lock-comment-delimiter-face "#65737e")

;; Highlight paren
;; (set-face-background 'show-paren-match "light blue")
;; (set-face-foreground 'show-paren-match "blue")
;; (set-face-attribute 'show-paren-match nil :weight 'normal)
;; End dark theme



;; Light theme
(load-theme 'mccarthy t)
(add-to-list 'load-path "~/.emacs.d/airline-themes")
(require 'airline-themes)
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
(set-face-attribute 'font-lock-constant-face nil :weight 'normal)

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
;; End light theme

(set-default-font "Anonymice Powerline-13")
(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)
