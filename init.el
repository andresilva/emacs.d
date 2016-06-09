;;; init.el --- Andre Silva's Emacs configuration

;;; Code:

;;; initialization

;; save start time so we can later measure the total loading time
(defconst emacs-start-time (current-time))

;; use one folder for all save/history/cache files
(defconst my-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

;; third-party code not available on ELPA should reside in this folder
(defconst my-sitelisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; temporarily reduce garbage collection during startup
(defconst my-initial-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 128 1000 1000))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my-initial-gc-cons-threshold)))

;; increase garbage collection threshold on minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold my-initial-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

;; load 3rd party code from `site-lisp' folder
(add-to-list 'load-path my-sitelisp-dir t)
(my-add-subdirs-to-load-path my-sitelisp-dir)

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

;; nice and smooth scrolling
(use-package smooth-scrolling
  :ensure t
  :diminish smooth-scrolling-mode
  :init
  (setq smooth-scroll-margin 5)
  (defun my-unset-scroll-margin ()
    "Set scroll-margin to zero."
    (setq-local scroll-margin 0))
  (add-hook 'messages-buffer-mode-hook 'my-unset-scroll-margin)
  :config
  (smooth-scrolling-mode)
  (enable-smooth-scroll-for-function previous-line)
  (enable-smooth-scroll-for-function next-line)
  (enable-smooth-scroll-for-function isearch-repeat)
  (enable-smooth-scroll-for-function scroll-down-command)
  (enable-smooth-scroll-for-function scroll-up-command)
  (enable-smooth-scroll-for-function evil-scroll-page-up)
  (enable-smooth-scroll-for-function evil-scroll-page-down))

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

;; internationalization
(prefer-coding-system 'utf-8)

;; clean up obsolete buffers
(use-package midnight)

;;;; theme

;; use the awesome `zenburn' as default theme
(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (load-theme 'zenburn :no-confirm)
  (when (fboundp 'fringe-mode)
    (fringe-mode '(8 . 0)))
  (set-face-attribute 'vertical-border
                      nil
                      :foreground "#4F4F4F"))

;;;; osx

;; setup modifier keys on OSX
(when (eq system-type 'darwin)
  (progn
    ;; fix cursor character deletion when Emacs loses focus
    (when (string-match "NS" (emacs-version))
      (defun my-fix-cursor ()
        (force-window-update))
      (add-hook 'focus-out-hook 'my-fix-cursor)
      (add-hook 'focus-in-hook 'my-fix-cursor))
    (when (memq window-system '(mac ns))
      (use-package exec-path-from-shell
        :ensure t
        :init
        (setq exec-path-from-shell-check-startup-files nil)
        :config
        (exec-path-from-shell-initialize)))
    ;; use "old-style" fullscreen
    (setq ns-use-native-fullscreen nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq ns-function-modifier 'hyper)
    ;; enable emoji, and stop the UI from freezing when trying to display them
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;;;; evil mode

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-normal-state-cursor '(box "DarkGoldenrod2")
        evil-insert-state-cursor '(bar "chartreuse3")
        evil-visual-state-cursor '(box "gray"))
  :config
  ;; replace evil insert state keymap with emacs'
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "s" 'save-buffer)
    (evil-leader/set-key "k" 'kill-this-buffer)
    (evil-leader/set-key "d" 'delete-window)
    (global-evil-leader-mode))
  (use-package evil-evilified-state
    :config
    ;; this is a bit dirty since it relies on the "private" evil-leader key map
    (define-key evil-evilified-state-map (kbd "<SPC>") evil-leader--default-map))
  (evil-mode)
  :bind ("C-q" . evil-execute-macro))

;;;; helm

;; project navigation
(use-package projectile
  :ensure t
  :diminish projectile-mode
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
    (evil-leader/set-key "pp" 'helm-projectile-switch-project)
    (evil-leader/set-key "pf" 'helm-projectile-find-file)
    (evil-leader/set-key "pa" 'helm-projectile-ag)
    :config
    (helm-projectile-on))
  (use-package helm-ag
    :ensure t)
  (use-package helm-flx
    :ensure t
    :config
    (helm-flx-mode +1))

  (defun my-helm-ff-filter-candidate-one-by-one (fcn file)
    (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
      (funcall fcn file)))

  (advice-add 'helm-ff-filter-candidate-one-by-one :around #'my-helm-ff-filter-candidate-one-by-one)

  (defun my-helm-file-completion-source-p (&rest args) t)

  (defun my-helm-find-files-up-one-level (fcn &rest args)
    (prog2
        (advice-add 'helm-file-completion-source-p :around #'my-helm-file-completion-source-p)
        (apply fcn args)
      (advice-remove 'helm-file-completion-source-p #'my-helm-file-completion-source-p)))

  (advice-add 'helm-find-files-up-one-level :around #'my-helm-find-files-up-one-level)

  (evil-leader/set-key "b" 'helm-mini)
  (evil-leader/set-key "f" 'helm-find-files)

  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action) ;; rebind tab to run persistent action
             ("C-i"   . helm-execute-persistent-action) ;; make TAB works in terminal
             ("C-z"   . helm-select-action) ;; list actions using C-z
             ("C-j"   . helm-next-line)
             ("C-k"   . helm-previous-line))

  :bind (("M-x"     . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b"   . helm-mini)
         ("M-y"     . helm-show-kill-ring)
         ("s-a"     . helm-projectile-ag)
         ("s-f"     . helm-projectile-find-file)))


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

;; nyan mode
(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-wavy-trail t)
  (setq nyan-minimum-window-width 128)
  :config
  (nyan-mode))

;; fancy mode line with `spaceline'
(use-package spaceline
  :ensure t
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq spaceline-window-numbers-unicode t)
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-install
   `(((window-number buffer-modified) :face highlight-face)
     anzu
     auto-compile
     (buffer-id remote-host)
     (projectile-root :when (not (derived-mode-p 'magit-mode)))
     major-mode
     nyan-cat
     (process :when active)
     ((flycheck-error flycheck-warning flycheck-info) :when active)
     ((minor-modes :separator spaceline-minor-modes-separator) :when active)
     (version-control :when active)
     (org-pomodoro :when active)
     (org-clock :when active))

   `(which-function
     (python-pyvenv :fallback python-pyenv)
     selection-info
     input-method
     (point-position
      line-column)
     (global :when active)
     buffer-position
     hud))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-flydiff-delay 1)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; highlight useful keywords
(use-package hl-todo
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

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
  :commands paradox-list-packages
  :init
  (setq paradox-github-token t)
  :config
  (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map))

;; start server if one isn't already running
(use-package server
  :diminish server-buffer-clients
  :config
  (unless (server-running-p)
    (server-start)))

;; undo and redo window layout operations
(use-package winner
  :config
  (winner-mode 1))

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

;; automatic resizing of windows to the golden ratio
(use-package golden-ratio
  :disabled t
  :ensure t
  :diminish golden-ratio-mode
  :init
  (setq window-combination-resize t)
  (setq golden-ratio-adjust-factor 0.81)
  (setq golden-ratio-exclude-modes '("bs-mode"
                                     "calc-mode"
                                     "ediff-mode"
                                     "dired-mode"
                                     "gud-mode"
                                     "gdb-locals-mode"
                                     "gdb-registers-mode"
                                     "gdb-breakpoints-mode"
                                     "gdb-threads-mode"
                                     "gdb-frames-mode"
                                     "gdb-inferior-io-mode"
                                     "gud-mode"
                                     "gdb-inferior-io-mode"
                                     "gdb-disassembly-mode"
                                     "gdb-memory-mode"
                                     "restclient-mode"))
  :config
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(select-window-0
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9
                  my-split-window-below-and-focus
                  my-split-window-right-and-focus)))
  (golden-ratio-mode 1))

;;; editor
;;;; settings

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; store `auto-save-list' backup file mapping in `savefile' dir
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my-savefile-dir))

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
  :diminish auto-revert-mode
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))

;; fill-column line length
(setq-default fill-column 100)

;; kill region or current line
(defun my-kill-region (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'my-kill-region)

;; save region or current line
(defun my-kill-ring-save (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'my-kill-ring-save)

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
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;; packages

;; edit remote files
(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (expand-file-name "tramp" my-savefile-dir)))

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
  :ensure t
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" my-savefile-dir))
  :commands mc/edit-lines)

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
        '(search-ring regexp-search-ring extended-command-history)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" my-savefile-dir))
  :config
  (savehist-mode +1))

;; `desktop' mode saves the state of Emacs from one session to another (buffers, frames, etc.)
(use-package desktop
  :init
  (setq desktop-path (list my-savefile-dir))
  (setq desktop-dirname my-savefile-dir))

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
  :ensure t
  :commands string-edit-at-point)

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
  :init
  (evil-leader/set-key "g" 'magit-status)
  :config
  (use-package evil-magit
    :ensure t
    :init
    (setq evil-magit-want-horizontal-movement t))
  :bind ("C-x g" . magit-status))

;; step through historic versions of git controlled files
(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

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
  (global-flycheck-mode))

;; show argument list/type information in the modeline
(use-package eldoc
  :diminish eldoc-mode)

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
(use-package scala-mode
  :ensure t
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(use-package ensime
  :ensure t
  :init
  (setq ensime-use-helm t)
  (setq ensime-sem-high-enabled-p nil)
  (setq user-emacs-ensime-directory (expand-file-name "ensime" my-savefile-dir))
  (setq sbt:sbt-prompt-regexp "^.*>[ ]*"
        sbt:prompt-regexp "^\\(\\(scala\\|.*\\)?>\\|[ ]+|\\)[ ]*")
  (evil-set-initial-state 'sbt-mode 'insert)
  (evil-leader/set-key-for-mode 'scala-mode "pc" 'ensime-sbt-do-compile)
  (evil-leader/set-key-for-mode 'scala-mode "." 'ensime-edit-definition)

  (defun my-configure-ensime ()
    "Ensure the file exists before starting `ensime-mode'."
    (cond
     ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (ensime-mode +1))
     ((buffer-file-name)
      (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

  ;; local function decoration/overriding
  (use-package noflet
    :ensure t)

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

  :config
  (defun ensime-modeline-string ()
    (when ensime-mode
      (condition-case err
          (let ((conn (ensime-connection-or-nil)))
            (cond ((and ensime-mode (not conn))
                   " E()")

                  ((and ensime-mode (ensime-connected-p conn))
                   (concat " E"
                           (let ((status (ensime-modeline-state-string conn))
                                 (unready (not (ensime-analyzer-ready conn))))
                             (cond (status (concat " [" status "] "))
                                   (unready " [Analyzing] ")
                                   (t "")))
                           (concat (format "(%s/%s)"
                                           (ensime-num-errors conn)
                                           (ensime-num-warnings conn)))))
                  (ensime-mode " E(Dead Connection)]")))
        (error (progn " E(wtf)"))))))

;;;; rust

;; `rust' programming mode
(use-package rust-mode
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "CARGO_HOME")
    (exec-path-from-shell-copy-env "RUST_SRC_PATH"))
  (use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))
  (use-package racer
    :ensure t
    :diminish racer-mode
    :init
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (evil-leader/set-key-for-mode 'rust-mode "." 'racer-find-definition))
  (use-package flycheck-rust
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
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
    :disabled t
    :config
    (add-hook 'js2-mode-hook 'tern-mode))
  (use-package company-tern
    :ensure t
    :disabled t
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
(use-package outline-magic
  :ensure t
  :diminish outline-minor-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :config
  (bind-key "C-<tab>" 'outline-cycle outline-minor-mode-map))

;; interactive lisp macro expansion
(use-package macrostep
  :ensure t
  :commands macrostep-expand
  :init
  (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map))

(bind-key "C-c C-e" 'eval-region emacs-lisp-mode-map)

;;;; shell

(use-package sh-script
  :init
  (setq sh-basic-offset 2)
  (defun my-init-sh-mode ()
    (when (and buffer-file-name
               (string-match-p "\\.zsh\\'" buffer-file-name))
      (sh-set-shell "zsh")))
  (add-hook 'sh-mode-hook 'my-init-sh-mode)
  ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms
  (dolist (pattern '("\\.zsh\\'"
                     "zlogin\\'"
                     "zlogout\\'"
                     "zpreztorc\\'"
                     "zprofile\\'"
                     "zshenv\\'"
                     "zshrc\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode))))

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
  :init
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil)
  :config
  (use-package company-web
    :ensure t
    :config
    (my-add-company-backend-with-yasnippet 'company-web-html))
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

;;;; lua

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lua-indent-level 2
        lua-indent-string-contents t))

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
  ;; create a logbook entry
  (setq org-log-into-drawer t)
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
  ;; move org-clock file to `savefile' dir
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
  ;; diminish org-indent-mode
  (use-package org-indent
    :diminish org-indent-mode)
  ;; pomodoro technique for org tasks
  (use-package org-pomodoro
    :ensure t)
  ;; fancy list bullets
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  ;; personal journal
  (use-package org-journal
    :ensure t
    :init
    (setq org-journal-dir "~/org/journal")
    (setq org-journal-enable-encryption t))

  ;; evil keybindings for `org-mode'
  (use-package evil-org
    :ensure t
    :diminish evil-org-mode)

  :mode ("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("<f6>"  . org-agenda)
         ("<f7>"  . org-clock-goto)
         ("<f8>"  . org-clock-in)
         ("<f9>"  . org-pomodoro)))

;;;; term

(add-hook 'term-mode-hook
          (lambda ()
            (setq yas-dont-activate t)))

;;;; gnus

(use-package gnus
  :defer t
  :commands gnus
  :init
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "gmane" (nntp-address "news.gmane.org"))))

  (evilified-state-evilify gnus-group-mode gnus-group-mode-map)
  (evilified-state-evilify gnus-server-mode gnus-server-mode-map)
  (evilified-state-evilify gnus-browse-mode gnus-browse-mode-map)
  (evilified-state-evilify gnus-article-mode gnus-article-mode-map)
  (evilified-state-evilify gnus-summary-mode gnus-summary-mode-map
    (kbd "J") 'gnus-summary-next-article
    (kbd "K") 'gnus-summary-prev-article))

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
  "Insert an at sign into the buffer."
  (interactive)
  (insert "@"))

(defun my-insert-euro-sign ()
  "Insert an euro sign into the buffer."
  (interactive)
  (insert "€"))

(defun my-toggle-frame-fullscreen ()
  "Enable old-style OSX fullscreen support in Emacs Mac port."
  (interactive)
  (if (string-match "Carbon" (emacs-version))
      (set-frame-parameter nil 'fullscreen
                           (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
    (toggle-frame-fullscreen)))

(defun my-toggle-window-focus ()
  "Toggle focus of the current window, i.e. delete all other windows and revert afterwards."
  (interactive)
  (if (bound-and-true-p my-window-focused)
      (progn
        (setq my-window-focused nil)
        (winner-undo))
    (progn
      (setq my-window-focused t)
      (delete-other-windows))))

(defun my-layout-triple-columns ()
  "Set the layout to triple columns."
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun my-layout-double-columns ()
  "Set the layout to double columns."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my-rotate-windows (arg)
  "Rotate existing windows, use the prefix ARG to rotate in the other direction."
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(defun my-split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down 1))

(defun my-split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right 1))

(bind-keys ("C-x ç"   . my-comment-or-uncomment-region-or-line)
           ("M-2"     . my-insert-at-sign)
           ("M-3"     . my-insert-euro-sign)
           ("<f5>"    . my-toggle-frame-fullscreen)
           ("C-c f"   . my-toggle-window-focus)
           ("C-x 2"   . my-split-window-below-and-focus)
           ("C-x 3"   . my-split-window-right-and-focus)
           ("C-c 2"   . my-layout-double-columns)
           ("C-c 3"   . my-layout-triple-columns)
           ("s-l"     . goto-line)
           ("C-c C-m" . execute-extended-command))

(add-hook 'after-init-hook 'my-toggle-frame-fullscreen)

;; print the load time
(when window-system
  (add-hook 'after-init-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time)
                                                        emacs-start-time))))
                (message "init finished [%.3fs]" elapsed)))))
