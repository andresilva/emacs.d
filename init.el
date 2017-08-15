;; save start time
(defconst start-time (current-time))

;; reduce garbage collections during init
(defconst init-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 128 1000 1000))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold init-gc-cons-threshold)))

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

;; better emacs defaults
(use-package better-defaults
  :ensure t)

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

;; print init time
(add-hook 'after-init-hook
          (lambda ()
            (let ((elapsed (float-time (time-subtract (current-time)
                                                      start-time))))
              (message "init finished [%.3fs]" elapsed))))
