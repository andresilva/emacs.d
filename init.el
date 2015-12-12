;; setup `package' but do not auto-load installed packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; `use-package' is not needed at runtime
(eval-when-compile
  (require 'use-package))

;; required for the `use-package' :bind keyword
(require 'bind-key)

;; reduce modeline clutter
(require 'diminish)

;; set default font
(set-default-font
 (apply 'font-spec :name "Source Code Pro" '(:size 13 :weight normal :width normal)) nil t)

;; set fallback font
(when (fboundp 'set-fontset-font)
  ;; window numbers
  (set-fontset-font "fontset-default"
		    '(#x2776 . #x2793) "Menlo")
  ;; mode-line circled letters
  (set-fontset-font "fontset-default"
		    '(#x24b6 . #x24fe) "Menlo")
  ;; mode-line additional characters
  (set-fontset-font "fontset-default"
		    '(#x2295 . #x22a1) "Menlo")
  ;; new version lighter
  (set-fontset-font "fontset-default"
		    '(#x2190 . #x2200) "Menlo"))

;; disable scrollbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menubar
(menu-bar-mode -1)

;; disable cursor blinking
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; kill region or current line
(require 'rect)
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end) rectangle-mark-mode)
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; use the awesome `zenburn' as default theme
(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (load-theme 'zenburn :no-confirm))

;; project navigation
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

;; interactive completion
(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :init
  (setq helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	helm-ff-search-library-in-sexp t
	helm-ff-file-name-history-use-recentf t)
  ;; enable fuzzy matching
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-M-x-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-apropos-fuzzy-match t
	helm-lisp-fuzzy-completion t
	helm-mode-fuzzy-match t
	helm-completion-in-region-fuzzy-match t)
  :config
  (require 'helm-config)
  (helm-mode)
  (helm-autoresize-mode)
  (use-package helm-projectile
    :ensure t
    :init
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match t)
    :config
    (helm-projectile-on))

  :bind (("M-x"     . helm-M-x)
	 ("C-x C-m" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b"   . helm-mini)
	 ("M-y"     . helm-show-kill-ring)
	 ("S-a"     . helm-projectile-ag)
	 ("S-f"     . helm-projectile-find-file)))

;; the best git client ever
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; modal editing
(use-package god-mode
  :ensure t
  :diminish god-local-mode
  :init
  (defun my-update-cursor ()
    (set-cursor-color
     (if (bound-and-true-p god-local-mode)
	 "chartreuse3"
       "DarkGoldenrod2")))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)

  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

;; ergonomic shortcuts
(use-package key-chord
  :ensure t
  :init
  (setq key-chord-two-keys-delay 0.2)
  (setq key-chord-one-key-delay 0.3)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jj" 'god-mode-all)
  (key-chord-define-global "jk" 'god-mode-all))

;; smooth scrolling
(use-package smooth-scrolling
  :ensure t)

;; enhanced `isearch'
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode))

;; window numbering and switching
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

;; `scala' programming mode
(use-package scala-mode2
  :ensure t
  :config
  (use-package ensime
    :ensure t
    :init
    (setq ensime-sem-high-enabled-p nil)
    :config
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

  :mode (("\\.scala\\'" . scala-mode)
	 ("\\.sbt\\'"   . scala-mode)))

;; spell checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; fancy mode line with `spaceline'
(use-package spaceline
  :ensure t
  :init
  (defun my-spaceline-highlight-face-god-state ()
    (if (bound-and-true-p god-local-mode)
	'spaceline-evil-insert
      'spaceline-evil-normal))
  (setq spaceline-highlight-face-func 'my-spaceline-highlight-face-god-state)

  (setq spaceline-window-numbers-unicode t)
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode))
