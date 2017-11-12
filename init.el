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
  :diminish undo-tree-mode)

;; extensible vi layer
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

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
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode))

;; the best git client ever
(use-package magit
  :ensure t
  :commands magit-status)

;; magit `evil' integration
(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

;; convenient key definitions
(use-package general
  :ensure t
  :config
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
   "gs" 'magit-status))

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
