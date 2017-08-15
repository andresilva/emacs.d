;; save start time
(defconst !/start-time (current-time))

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

;; `M-x' enhancement
(use-package smex
  :ensure t
  :defer t
  :init
  (setq smex-history-length 32))

;; fuzzy matching
(use-package flx
  :ensure t
  :defer t)

;; generic completion frontend
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))

;; project interaction library
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  (use-package counsel-projectile
    :ensure t))

;; a git porcelain
(use-package magit
  :ensure t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :commands magit-status
  :config
  (use-package evil-magit
    :ensure t))

;; display available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

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

   "b" '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "bs" '!/switch-to-scratch-buffer
   "bm" '!/switch-to-messages-buffer

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

   "h" '(:ignore t :which-key "help")
   "hi" '(counsel-info-lookup-symbol :which-key "describe-symbol")
   "hf" '(counsel-describe-function :which-key "describe-function")
   "hv" '(counsel-describe-variable :which-key "describe-variable")
   "hk" 'describe-key

   "gs" 'magit-status))

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

(defun !/package-used-p (package)
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
        (when (and (!/package-used-p 'projectile)
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
                 (when (and (!/package-used-p 'projectile)
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

;; print init time
(add-hook 'after-init-hook
          (lambda ()
            (let ((elapsed (float-time (time-subtract (current-time)
                                                      !/start-time))))
              (message "init finished [%.3fs]" elapsed))))
