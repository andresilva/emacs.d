;; save start time
(defconst !/start-time (current-time))

;; reduce garbage collections during init
(defconst !/initial-gc-cons-threshold gc-cons-threshold)
(setq-default gc-cons-threshold (* 128 1000 1000))
(add-hook 'after-init-hook
          (lambda () (setq-default gc-cons-threshold !/initial-gc-cons-threshold)))

;;; setup `package'
(require 'package)
(setq-default package-enable-at-startup nil)
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

;; configure `exec-path' from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq-default exec-path-from-shell-check-startup-files nil)
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
  (setq-default evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; `M-x' enhancement
(use-package smex
  :ensure t
  :defer t
  :init
  (setq-default smex-history-length 32))

;; fuzzy matching
(use-package flx
  :ensure t
  :defer t)

;; generic completion frontend
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq-default
   ivy-use-virtual-buffers t
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))

;; project interaction library
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq-default projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;; `ivy' integration for `projectile'
(use-package counsel-projectile
  :ensure t
  :after projectile)

;; tagged workspaces
(use-package persp-mode
  :ensure t
  :init
  (setq-default
   persp-nil-name "nil"
   persp-is-ibc-as-f-supported nil
   persp-reset-windows-on-nil-window-conf nil
   persp-set-last-persp-for-new-frames nil
   persp-auto-resume-time -1)
  (add-hook 'after-init-hook
            (lambda ()
              (persp-mode 1)
              (add-hook 'ivy-ignore-buffers #'!//layout-not-contains-buffer-p)
              (setq-default
               ivy-sort-functions-alist (append ivy-sort-functions-alist
                                                '((persp-kill-buffer . nil)
                                                  (persp-remove-buffer . nil)
                                                  (persp-add-buffer . nil)
                                                  (persp-switch . nil)
                                                  (persp-window-switch . nil)
                                                  (persp-frame-switch . nil)))))))

;; window numbering and switching
(use-package winum
  :ensure t
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

;; a git porcelain
(use-package magit
  :ensure t
  :commands magit-status
  :init
  (setq-default magit-completing-read-function 'ivy-completing-read))

;; `evil' keys for `magit'
(use-package evil-magit
  :ensure t
  :after magit)

;; display available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

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
  (general-define-key :keymaps 'ivy-minibuffer-map
                      "C-j" 'ivy-next-line
                      "C-k" 'ivy-previous-line
                      "C-h" (kbd "DEL")
                      "C-S-h" 'help-map
                      "C-l" 'ivy-alt-done
                      "TAB" 'ivy-alt-done
                      "C-M-l" 'ivy-immediate-done
                      "<C-tab>" 'ivy-immediate-done
                      "<escape>" 'minibuffer-keyboard-quit)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :which-key "M-x")
   "TAB" '!/alternate-buffer

   "0" 'winum-select-window-0-or-10
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9

   "b" '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "bs" '!/switch-to-scratch-buffer
   "bm" '!/switch-to-messages-buffer

   "ba" 'persp-add-buffer
   "br" 'persp-remove-buffer
   "bB" '!/non-restricted-buffer-list-ivy

   "f" '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "find-file")
   "fs" 'save-buffer
   "fr" 'counsel-recentf
   "fD" '!/delete-current-buffer-file
   "fR" '!/rename-current-buffer-file

   "r" '(:ignore t :which-key "registers")
   "rl" 'ivy-resume
   "ry" 'counsel-yank-pop
   "rm" 'counsel-mark-ring

   "p" '(:ignore t :which-key "projects")
   "p SPC" 'counsel-projectile
   "pb" 'counsel-projectile-switch-to-buffer
   "pd" 'counsel-projectile-find-dir
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file
   "ps" 'counsel-projectile-rg
   "pl" '!/ivy-layout-switch-project

   "l" '(:ignore t :which-key "layouts")
   "ll" '(!/ivy-layouts :which-key "list-layouts")
   "lc" '(persp-kill-without-buffers :which-key "close-layout")
   "lk" '(persp-kill :which-key "kill-layout")

   "h" '(:ignore t :which-key "help")
   "ha" '(counsel-apropos :which-key "apropos")
   "hi" '(counsel-info-lookup-symbol :which-key "describe-symbol")
   "hf" '(counsel-describe-function :which-key "describe-function")
   "hv" '(counsel-describe-variable :which-key "describe-variable")
   "hk" 'describe-key

   "w" '(:ignore t :which-key "windows")
   "w TAB" '!/alternate-window
   "w2" '!/window-layout-double-columns
   "w3" '!/window-layout-triple-columns
   "wb" '!/switch-to-minibuffer-window
   "wd" '!/delete-window
   "wo" 'other-frame
   "ws" 'split-window-below
   "w-" 'split-window-below
   "wv" 'split-window-right
   "w/" 'split-window-right
   "w=" 'balance-windows
   "ww" '!/alternate-window

   "c" '!/comment-or-uncomment-region-or-line
   "g" 'magit-status
   "u" 'universal-argument
   "v" 'er/expand-region
   ))

(defun !/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun !/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(defun !/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
If prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))))

(defun !/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
If prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun !//package-used-p (package)
  "Return `t' if PACKAGE is used (i.e. it is available), `nil' otherwise."
  (when (require package nil 'noerror)
    t))

(defun !/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (!//package-used-p 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun !/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting. If the
buffer isn't visiting a file, ask if it should be saved to a
file, or just renamed. If called without a prefix argument, the
prompt is initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (!//package-used-p 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun !/window-layout-double-columns ()
  "Set the window layout to double columns."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun !/window-layout-triple-columns ()
  "Set the window layout to triple columns."
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun !/switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun !/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun !/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun !//layout-not-contains-buffer-p (buffer)
  "Return non-nil if current layout doesn't contain BUFFER."
  (not (persp-contain-buffer-p buffer)))

(defun !/non-restricted-buffer-list-ivy ()
  (interactive)
  (let ((ivy-ignore-buffers (remove #'!//layout-not-contains-buffer-p ivy-ignore-buffers)))
    (ivy-switch-buffer)))

(defun !/ivy-layout-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to project layout: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action (lambda (project)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch
                         (file-name-nondirectory (directory-file-name (expand-file-name project))))
                        (let ((projectile-completion-system 'ivy))
                          (projectile-switch-project-by-name project))))))

(defun !/ivy-layouts ()
  (interactive)
  (ivy-read "Layouts: "
            (persp-names)
            :caller '!/ivy-layouts
            :action (lambda (name)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch name)))))

;; print init time
(add-hook 'after-init-hook
          (lambda ()
            (let ((elapsed (float-time (time-subtract (current-time)
                                                      !/start-time))))
              (message "init finished [%.3fs]" elapsed))))
