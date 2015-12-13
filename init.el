;;; init.el --- Andre Silva's Emacs configuration

;;; Code:

;;; initialization

;; save start time so we can later measure the total loading time
(defconst emacs-start-time (current-time))

;; reduce the frequency of garbage collection by making it happen on
;; every 20MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 20 1000 1000))

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

;; required for the `use-package' :diminish keyword
;; reduce modeline clutter
(require 'diminish)

;;; ui
;;;; font

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

;;;; settings

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

;; clean up obsolete buffers
(use-package midnight)

;;;; theme

;; use the awesome `zenburn' as default theme
(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (load-theme 'zenburn :no-confirm))

;;;; osx

;; setup modifier keys on OSX
(when (eq system-type 'darwin)
  (progn
    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq ns-function-modifier 'hyper)))

;;;; helm

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
         ("s-a"     . helm-projectile-ag)
         ("s-f"     . helm-projectile-find-file)))

;;;; god mode

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
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jk" 'god-mode-all))

;;;; mode line

;; window numbering and switching
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

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

;;;; extras

;; show line diff indicator on fringe
(use-package diff-hl
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (global-diff-hl-mode))

;; highlight useful keywords
(use-package hl-todo
  :ensure t
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

;; enhanced `dired'
(use-package dired+
  :defer t
  :ensure t)

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

;; start server if one isn't already running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;; editor
;;;; settings

;; delete selection with a keypress
(delete-selection-mode t)

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

;; use `hippie-expand' instead of `dabbrev'
(bind-keys ("M-/" . hippie-expand)
           ("C-ç" . hippie-expand))

;; revert buffers automatically (also enabled for non-file buffers)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; fill-column line length
(setq-default fill-column 100)

;; kill region or current line
(use-package rect
  :config
  (defadvice kill-region (before smart-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end) rectangle-mark-mode)
       (list (line-beginning-position)
             (line-beginning-position 2))))))

;;;; white space

;; no tabs please (but make it look like it)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; newline at end of file
(setq require-final-newline t)

;; keep white space tidy
(use-package whitespace
  :diminish whitespace-mode
  :init
  (defun my-enable-whitespace-mode ()
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)
    (whitespace-mode +1))
  (add-hook 'text-mode-hook 'my-enable-whitespace-mode)
  (add-hook 'prog-mode-hook 'my-enable-whitespace-mode)
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;;;; spell checking

(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;; packages

;; multiple cursors are easier than macros
(use-package multiple-cursors
  :ensure t)

;; in-buffer auto completion framework
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  :config
  (global-company-mode))

;;; major modes
;;;; git

;; the best git client ever
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;; programming

;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-indication-mode nil)
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

;;;; scala

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

;;;; emacs lisp

;; fold my `init.el' like an org file
(use-package outshine
  :ensure t
  :diminish outline-minor-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

;;;; org

;; `org-mode'
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)))

;;; defuns

(defun my-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my-insert-at-sign ()
  "Inserts an at sign into the buffer."
  (interactive)
  (insert "@"))

(defun my-insert-euro-sign ()
  "Inserts an euro sign into the buffer."
  (interactive)
  (insert "€"))

(defun my-toggle-fullscreen ()
  "Toggles the current window to full screen. Supports both OSX
and X11 environment. On OSX it does so using the 'old-style'
fullscreen."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
   ((eq window-system 'x)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
   (error "Unable to toggle fullscreen")))

(defun my-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.  Move
point to the first non-whitespace character on this line.  If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.  If ARG is not nil or 1, move forward
ARG - 1 lines first. If point reaches the beginning or end of the
buffer, stop there."
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

(bind-keys ("C-x ç"   . my-comment-or-uncomment-region-or-line)
           ("C-a"     . my-move-beginning-of-line)
           ("s-2"     . my-insert-at-sign)
           ("s-3"     . my-insert-euro-sign)
           ("<f5>"    . my-toggle-fullscreen)
           ("s-l"     . goto-line)
           ("C-c C-m" . execute-extended-command))

;; print the load time
(when window-system
  (add-hook 'after-init-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time)
                                                        emacs-start-time))))
                (message "init finished [%.3fs]" elapsed)))))
