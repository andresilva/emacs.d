;; save start time
(defconst !/start-time (current-time))

;; use one folder for all save/history/cache files
(defconst !/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p !/savefile-dir)
  (make-directory !/savefile-dir))

;; third-party code not available on ELPA should reside in this folder
(defconst !/sitelisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; load 3rd party code from `site-lisp' folder
(add-to-list 'load-path !/sitelisp-dir t)

;; use a separate file for emacs customization settings
(defconst !/custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file !/custom-file)

;; reduce garbage collections during init
(defconst !/initial-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 128 1000 1000))
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold !/initial-gc-cons-threshold)))

;;; setup `package'
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; `use-package' is not needed at runtime
(eval-when-compile
  (require 'use-package))

;; required for `use-package' :diminish
;; reduce modeline clutter
(use-package diminish)

;; start `server' if one isn't already running
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; configure `exec-path' from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

;; unobtrusively trim trailing whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

;; extensible vi layer
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; `anzu' for `evil' mode
(use-package evil-anzu
  :ensure t
  :after evil)

;; dark theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; interactive completion
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1))

;; numbered window shortcuts
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

;; project interaction library
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm
	projectile-cache-file (expand-file-name "projectile.cache" !/savefile-dir)
	projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" !/savefile-dir))
  :config
  (projectile-mode))

;; the best git client ever
(use-package magit
  :ensure t
  :commands magit-status)

;; highlight uncommitted changes on the left side of the window
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter:window-width 1
	git-gutter:update-interval 2
	git-gutter:modified-sign "│"
	git-gutter:added-sign "│"
	git-gutter:deleted-sign "│")
  :config
  (set-face-foreground 'git-gutter:modified "#ffb86c") ; dracula rainbow-5
  (set-face-foreground 'git-gutter:added "#50fa7b") ; dracula rainbow-6
  (set-face-foreground 'git-gutter:deleted "#ff5555") ; dracula rainbow-9
  (global-git-gutter-mode t))

;; `evil' keys for `magit'
(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

;; display available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; fancy mode-line icons
(use-package all-the-icons
  :ensure t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; store `auto-save-list' backup file mapping in `savefile' dir
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" !/savefile-dir))

;; setup `hippie-expand' expand functions
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

;; disable startup screen
(setq inhibit-startup-screen t)

;; highlight current line
(global-hl-line-mode 1)

;; internationalization
(prefer-coding-system 'utf-8)

;; revert buffers automatically (also non-file buffers)
(use-package autorevert
  :diminish auto-revert-mode
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))

;; clean up obsolete buffers
(use-package midnight)

;; meaningful names for buffers with the same name
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; `saveplace' remembers your location in a file when saving files
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" !/savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

;; `savehist' keeps track of some history
(use-package savehist
  :init
  (setq savehist-additional-variables
	;; search entries
	'(search-ring regexp-search-ring extended-command-history)
	;; save every minute
	savehist-autosave-interval 60
	;; keep the home clean
	savehist-file (expand-file-name "savehist" !/savefile-dir))
  :config
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" !/savefile-dir)
	recentf-max-saved-items 500
	recentf-max-menu-items 15
	;; disable recentf-cleanup on Emacs start, because it can cause
	;; problems with remote files
	recentf-auto-cleanup 'never)
  (defun !/recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
		(string-prefix-p dir file-dir))
	      (mapcar 'file-truename (list !/savefile-dir package-user-dir)))))
  :config
  (add-to-list 'recentf-exclude '!/recentf-exclude-p)
  (recentf-mode +1))

;; disabled right fringe and small left fringe
(when (fboundp 'fringe-mode)
  (fringe-mode '(4 . 0)))

;; disable scrollbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; customize mode-line
(defun !/modeline-segment-git-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
		 'face `(:height 1 :family ,(all-the-icons-octicon-family))
		 'display '(raise 0))
     (propertize (format " %s" branch)))))

(defun !/modeline-segment-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud"))
		 'face `(:height 1)
		 'display '(raise 0))
     (propertize (format " %s" revision) 'face `(:height 0.9)))))

(defvar !/mode-line-vc
  '(:eval (when vc-mode
	    (cond
	     ((string-match "Git[:-]" vc-mode) (!/modeline-segment-git-vc))
	     ((string-match "SVN-" vc-mode) (!/modeline-segment-svn-vc))
	     (t (format "%s" vc-mode))))
	  face mode-line-directory)
  "Formats the current directory.")

(setq evil-mode-line-format '(before . mode-line-position))
(setq-default mode-line-format
	      (list
	       '(:eval (propertize (format " %s" (window-numbering-get-number-string))))
	       " "
	       mode-line-mule-info
	       mode-line-modified
	       mode-line-frame-identification
	       mode-line-buffer-identification
	       "  "
	       mode-line-position
	       !/mode-line-vc
	       "   "
	       mode-line-modes))

;; increase mode-line width
(custom-set-faces
 `(mode-line-inactive ((t (:background ,(face-attribute 'mode-line-inactive :background)
				       :foreground ,(face-attribute 'mode-line-inactive :foreground)
				       :box (:line-width 4 :color ,(face-attribute 'mode-line-inactive :background))))))
 `(mode-line ((t (:background ,(face-attribute 'mode-line :background)
			      :foreground ,(face-attribute 'mode-line :foreground)
			      :box (:line-width 4 :color ,(face-attribute 'mode-line :background)))))))

;; semantic region expansion
(use-package expand-region
  :ensure t
  :commands er/expand-region
  :init
  (setq-default
   expand-region-contract-fast-key "V"
   expand-region-reset-fast-key "r"))

;; convenient key definitions
(use-package general
  :ensure t
  :config
  (general-define-key
   "M-/" 'hippie-expand
   "C-;" 'hippie-expand)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "SPC" '(helm-M-x :which-key "M-x")

   ;; window numbers
   "0" 'select-window-0
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   "4" 'select-window-4
   "5" 'select-window-5
   "6" 'select-window-6
   "7" 'select-window-7
   "8" 'select-window-8
   "9" 'select-window-9

   ;; git
   "gs" 'magit-status

   "v" 'er/expand-region))

;; load emacs customization settings
(if (file-exists-p custom-file)
    (load custom-file))

;; print init time
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((elapsed (float-time (time-subtract (current-time)
						      !/start-time))))
	      (message "init finished [%.3fs]" elapsed)))
	  t)
