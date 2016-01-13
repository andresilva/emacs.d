;;; init.el --- Andre Silva's Emacs configuration

;;; Code:

;;; initialization

;; save start time so we can later measure the total loading time
(defconst emacs-start-time (current-time))

;; use one folder for all save/history files
(defvar my-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

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

(eval-when-compile (require 'cl))
(defun my-add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

;; load 3rd party code from `site-lisp' folder
(my-add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

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

;; nice scrolling
(setq scroll-margin 5
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

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

;; highlight current line
(global-hl-line-mode 1)

;; clean up obsolete buffers
(use-package midnight)

;;;; theme

;; use the awesome `zenburn' as default theme
(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (load-theme 'zenburn :no-confirm)
  (set-face-attribute 'vertical-border
                      nil
                      :foreground "#4F4F4F"))

;;;; osx

;; setup modifier keys on OSX
(when (eq system-type 'darwin)
  (progn
    ;; fix cursor character deletion when Emacs loses focus
    (defun my-fix-cursor ()
      (force-window-update))
    (add-hook 'focus-out-hook 'my-fix-cursor)
    (add-hook 'focus-in-hook 'my-fix-cursor)
    (use-package exec-path-from-shell
      :ensure t
      :init
      (setq exec-path-from-shell-check-startup-files nil)
      :config
      (exec-path-from-shell-initialize))
    ;; use "old-style" fullscreen
    (setq ns-use-native-fullscreen nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq ns-function-modifier 'hyper)
    ;; enable emoji, and stop the UI from freezing when trying to display them
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;;;; helm

;; project navigation
(use-package projectile
  :ensure t
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-savefile-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-savefile-dir))
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
  (use-package helm-ag
    :ensure t)
  (use-package helm-flx
    :ensure t
    :config
    (helm-flx-mode +1))

  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action) ;; rebind tab to run persistent action
             ("C-i"   . helm-execute-persistent-action) ;; make TAB works in terminal
             ("C-z"   . helm-select-action))            ;; list actions using C-z

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
         "SkyBlue2"
       "DarkGoldenrod2")))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)

  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  :config
  (my-update-cursor))

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
  (window-numbering-mode)
  (bind-keys :map window-numbering-keymap
             ("s-0" . select-window-0)
             ("s-1" . select-window-1)
             ("s-2" . select-window-2)
             ("s-3" . select-window-3)
             ("s-4" . select-window-4)
             ("s-5" . select-window-5)
             ("s-6" . select-window-6)
             ("s-7" . select-window-7)
             ("s-8" . select-window-8)
             ("s-9" . select-window-9))
  (unbind-key "M-0" window-numbering-keymap)
  (unbind-key "M-1" window-numbering-keymap)
  (unbind-key "M-2" window-numbering-keymap)
  (unbind-key "M-3" window-numbering-keymap)
  (unbind-key "M-4" window-numbering-keymap)
  (unbind-key "M-5" window-numbering-keymap)
  (unbind-key "M-6" window-numbering-keymap)
  (unbind-key "M-7" window-numbering-keymap)
  (unbind-key "M-8" window-numbering-keymap)
  (unbind-key "M-9" window-numbering-keymap))

;; fancy mode line with `spaceline'
(use-package spaceline
  :ensure t
  :init
  (defun my-spaceline-highlight-face-god-state ()
    (if (bound-and-true-p god-local-mode)
        'spaceline-evil-emacs
      'spaceline-evil-normal))
  (setq spaceline-highlight-face-func 'my-spaceline-highlight-face-god-state)

  (setq spaceline-window-numbers-unicode t)
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode))

;;;; extras

;; display key bindings popup given command prefix
(use-package which-key
  :ensure t
  :disabled t
  :diminish which-key-mode
  :config
  (which-key-mode))

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
(use-package dired-x)

;; enhanced `isearch'
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode))

;; enhanced `list-packages'
(use-package paradox
  :ensure t
  :init
  (setq paradox-github-token t)
  :commands paradox-list-packages)

;; start server if one isn't already running
(use-package server
  :diminish server-buffer-clients
  :config
  (unless (server-running-p)
    (server-start)))

;; use rvm ruby version
(defun my-init-rvm ()
  (use-package rvm
    :ensure t
    :config
    (rvm-use-default))
  (remove-hook 'ruby-mode-hook 'my-init-rvm)
  (remove-hook 'markdown-mode-hook 'my-init-rvm))
(add-hook 'ruby-mode-hook 'my-init-rvm)
(add-hook 'markdown-mode-hook 'my-init-rvm)

;; local function decoration/overriding
(use-package noflet
  :ensure t)

;;; editor
;;;; settings

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(use-package autorevert
  :init
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))

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

;; useful interactive commands to enhance the overall Emacs experience
(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  :bind (("C-c n" . crux-cleanup-buffer-or-region)
         ("C-a"   . crux-move-beginning-of-line)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c D" . crux-delete-file-and-buffer)
         ("s-k"   . crux-kill-whole-line)))

;; multiple cursors are easier than macros
(use-package multiple-cursors
  :commands mc/edit-lines
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" my-savefile-dir))
  :ensure t)

;; expand selected region by semantic units
(use-package expand-region
  :ensure t
  :bind ("C-{" . er/expand-region))

;; in-buffer auto completion framework
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode))

;; sensible undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))

;; smart pairing
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (show-smartparens-global-mode t))

;; meaningful names for buffers with the same name
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; `saveplace' remembers your location in a file when saving files
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

;; `savehist' keeps track of some history
(use-package savehist
  :init
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" my-savefile-dir))
  :config
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" my-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (defun my-recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
                (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list my-savefile-dir package-user-dir)))))
  :config
  (add-to-list 'recentf-exclude 'my-recentf-exclude-p)
  (recentf-mode +1))

;; avoid string escape nightmares
(use-package string-edit
  :commands string-edit-at-point
  :ensure t)

;; auto-save buffers when certain events happen, e.g. switching between buffers, frame loses focus
(use-package super-save
  :ensure t
  :config
  (super-save-initialize))

;;; major modes
;;;; git

;; the best git client ever
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; follow symlinks to version controlled folders without asking
(use-package vc
  :init
  (setq vc-follow-symlinks t))

;;;; programming

;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-indication-mode nil)
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; template code snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (defun my-company-backend-with-yasnippet (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (defun my-add-company-backend-with-yasnippet (backend)
    (add-to-list 'company-backends (my-company-backend-with-yasnippet backend)))
  :config
  ;; enable yasnippet completions on all company backends
  (setq company-backends (mapcar #'my-company-backend-with-yasnippet company-backends))
  (yas-global-mode 1)

  :bind ("C-x y" . yas-describe-tables))

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
    (defun my-configure-ensime ()
      "Ensure the file exists before starting `ensime-mode'."
      (cond
       ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (ensime-mode +1))
       ((buffer-file-name)
        (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

    (defun my-maybe-start-ensime ()
      (when (buffer-file-name)
        (let ((ensime-buffer (my-ensime-buffer-for-file (buffer-file-name)))
              (file (ensime-config-find-file (buffer-file-name)))
              (is-source-file (s-matches? (rx (or "/src/" "/test/")) (buffer-file-name))))

          (when (and is-source-file (null ensime-buffer))
            (noflet ((ensime-config-find (&rest _) file))
                    (save-window-excursion
                      (ensime)))))))

    (defun my-ensime-buffer-for-file (file)
      "Find the Ensime server buffer corresponding to FILE."
      (let ((default-directory (file-name-directory file)))
        (-when-let (project-name (projectile-project-p))
          (--first (-when-let (bufname (buffer-name it))
                     (and (s-contains? "inferior-ensime-server" bufname)
                          (s-contains? (file-name-nondirectory project-name) bufname)))
                   (buffer-list)))))

    (defun my-current-line ()
      "Return the line at point as a string."
      (buffer-substring (line-beginning-position) (line-end-position)))

    ;; prevent common flyspell false positives in scala-mode
    (defun my-flyspell-verify-scala ()
      (and (flyspell-generic-progmode-verify)
           (not (s-matches? (rx bol (* space) "package") (my-current-line)))))

    (defun my-configure-flyspell-scala ()
      (setq-local flyspell-generic-check-word-predicate 'my-flyspell-verify-scala))

    ;; don't use Scala checker if ensime mode is active, since it provides better error checking.
    (defun my-disable-flycheck-scala ()
      (push 'scala flycheck-disabled-checkers))

    ;; add yasnippet templates to ensime-company completions
    (defun my-enable-company-scala ()
      (my-add-company-backend-with-yasnippet 'ensime-company))

    (add-hook 'scala-mode-hook 'my-configure-flyspell-scala)
    (add-hook 'scala-mode-hook 'my-configure-ensime)
    (add-hook 'scala-mode-hook 'my-maybe-start-ensime)

    (add-hook 'ensime-mode-hook 'my-disable-flycheck-scala)
    (add-hook 'ensime-mode-hook 'my-enable-company-scala))

  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

;;;; rust

;; `rust' programming mode
(use-package rust-mode
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "RUST_SRC_PATH"))
  (use-package racer
    :ensure t
    :init
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode))
  (use-package company-racer
    :ensure t
    :config
    (my-add-company-backend-with-yasnippet 'company-racer))
  :mode ("\\.rust\\'" . rust-mode))

;; needed for `cargo' files
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;;;; javascript

;; `javascript' programming mode
(use-package js2-mode
  :ensure t
  :init
  (setq-default js2-basic-offset 2)
  :config
  (use-package tern
    :ensure t
    :config
    (add-hook 'js2-mode-hook 'tern-mode))
  (use-package company-tern
    :ensure t
    :config
    (my-add-company-backend-with-yasnippet 'company-tern))
  :mode ("\\.js\\'" . js2-mode))

;; `json' mode
(use-package json-mode
  :ensure t
  :init
  (setq-default js-indent-level 2)
  :mode ("\\.json\\'" . json-mode))

;;;; emacs lisp

;; fold my `init.el' like an org file
(use-package outshine
  :ensure t
  :diminish outline-minor-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

;; interactive lisp macro expansion
(use-package macrostep
  :ensure t
  :commands macrostep-expand
  :init
  (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map))

;;;; shell

(use-package sh-script
  :defer t
  :init
  ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms
  (dolist (pattern '("\\.zsh\\'"
                     "zlogin\\'"
                     "zlogout\\'"
                     "zpreztorc\\'"
                     "zprofile\\'"
                     "zshenv\\'"
                     "zshrc\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
  (defun my-init-sh-mode ()
    (when (and buffer-file-name
               (string-match-p "\\.zsh\\'" buffer-file-name))
      (sh-set-shell "zsh")))
  (add-hook 'sh-mode-hook 'my-init-sh-mode))

;;;; markdown

;; setup `markdown-mode' to render with redcarpet
(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "redcarpet --parse tables")
  (setq markdown-command-needs-filename t)
  :mode ("\\.md\\'" . markdown-mode))

;;;; yaml

;; `yaml' mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'"            . yaml-mode)))

;;;; web

;; web programming mode
(use-package web-mode
  :ensure t
  :config
  (use-package company-web
    :ensure t
    :config
    (my-add-company-backend-with-yasnippet 'company-web-html))
  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil)
  (sp-with-modes '(web-mode)
                 (sp-local-pair "%" "%"
                                :unless '(sp-in-string-p)
                                :post-handlers '(((lambda (&rest _ignored)
                                                    (just-one-space)
                                                    (save-excursion (insert " ")))
                                                  "SPC" "=" "#")))
                 (sp-local-pair "<% "  " %>" :insert "C-c %")
                 (sp-local-pair "<%= " " %>" :insert "C-c =")
                 (sp-local-pair "<%# " " %>" :insert "C-c #")
                 (sp-local-tag "%" "<% "  " %>")
                 (sp-local-tag "=" "<%= " " %>")
                 (sp-local-tag "#" "<%# " " %>"))
  :mode (("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.twig\\'"       . web-mode)
         ("\\.html\\'"       . web-mode)
         ("\\.htm\\'"        . web-mode)
         ("\\.[gj]sp\\'"     . web-mode)
         ("\\.as[cp]x?\\'"   . web-mode)
         ("\\.eex\\'"        . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.eco\\'"        . web-mode)
         ("\\.jsx\\'"        . web-mode)
         ("\\.ejs\\'"        . web-mode)
         ("\\.djhtml\\'"     . web-mode)))

;;;; rest client

;; interact with HTTP APIs
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;;;; org

;; `org-mode'
(use-package org
  :init
  ;; log time when task is finished
  (setq org-log-done 'time)
  ;; org directory and agenda files
  (setq org-directory "~/org")
  (setq org-agenda-files (quote ("~/org/todo.org"
                                 "~/org/projects"
                                 "~/org/journal")))
  (setq org-default-notes-file "~/org/refile.org")
  ;; include numeric filenames in regexp (for journal files)
  (setq org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+")
  ;; org keywords and faces
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)"))))
  (setq org-todo-keyword-faces
        (quote
         ;; normal org-mode workflow
         (("TODO" :foreground "red" :weight bold)
          ("STARTED" :foreground "dodger blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELED" :foreground "forest green" :weight bold)
          ;; music queue workflow
          ("ADDED" :foreground "red" :weight bold)
          ("DOWNLOADED" :foreground "dodger blue" :weight bold)
          ("IMPORTED" :foreground "orange" :weight bold)
          ("LISTENED" :foreground "forest green" :weight bold))))
  ;; state triggers
  (setq org-todo-state-tags-triggers
        (quote (("CANCELED" ("CANCELED" . t))
                ("WAITING"  ("WAITING"  . t))
                ("HOLD"     ("WAITING"  . t) ("HOLD" . t))
                (done       ("WAITING")      ("HOLD"))
                ("TODO"     ("WAITING")      ("CANCELED") ("HOLD"))
                ("STARTED"  ("WAITING")      ("CANCELED") ("HOLD"))
                ("DONE"     ("WAITING")      ("CANCELED") ("HOLD")))))
  ;; use fast todo selection
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; move org-clock file to savefile dir
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" my-savefile-dir))
  ;; show lots of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 36)
  ;; resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; change tasks to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; set default column format
  (setq org-columns-default-format
        "%40ITEM %TODO %5Effort(Effort){:} %6CLOCKSUM")
  ;; enable org-indent-mode
  (setq org-startup-indented t)
  ;; handle empty lines
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))
  ;; templates
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file+headline "~/org/refile.org" "Tasks")
                 "* TODO %?")
                ("i" "Todo+Iteration" entry (file+headline "~/org/refile.org" "Tasks")
                 "* TODO %? %^{Iteration}p"))))
  ;; refiling
  ;; targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  ;; use full outline paths for refile targets - we file directly with ido
  (setq org-refile-use-outline-path t)
  ;; targets complete directly with ido
  (setq org-outline-path-complete-in-steps nil)
  ;; allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  ;; use ido for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  ;; babel
  ;; enable syntax highlighting
  (setq org-src-fontify-natively t)
  ;; do not prompt to confirm evaluation
  (setq org-confirm-babel-evaluate nil)
  ;; org-pomodoro
  ;; reduce volume of the bell sounds
  (setq org-pomodoro-start-sound-args "-v 0.3")
  (setq org-pomodoro-finished-sound-args "-v 0.3")
  (setq org-pomodoro-killed-sound-args "-v 0.3")
  (setq org-pomodoro-short-break-sound-args "-v 0.3")
  (setq org-pomodoro-long-break-sound-args "-v 0.3")
  (setq org-pomodoro-ticking-sound-args "-v 0.3")
  :config
  ;; resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; enable languages in babel
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((scheme . t)
           (sh     . t)
           (org    . t)
           (latex  . t))))
  ;; pomodoro technique for org tasks
  (use-package org-pomodoro
    :ensure t)
  ;; fancy list bullets
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode))
  ;; personal journal
  (use-package org-journal
    :ensure t
    :init
    (setq org-journal-dir "~/org/journal")
    (setq org-journal-enable-encryption t))

  :mode ("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("<f6>"  . org-agenda)
         ("<f7>"  . org-clock-goto)
         ("<f8>"  . org-clock-in)
         ("<f9>"  . org-pomodoro)))

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

(bind-keys ("C-x ç"   . my-comment-or-uncomment-region-or-line)
           ("M-2"     . my-insert-at-sign)
           ("M-3"     . my-insert-euro-sign)
           ("<f5>"    . toggle-frame-fullscreen)
           ("s-l"     . goto-line)
           ("C-c C-m" . execute-extended-command))

(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; print the load time
(when window-system
  (add-hook 'after-init-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time)
                                                        emacs-start-time))))
                (message "init finished [%.3fs]" elapsed)))))
