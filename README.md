# emacs.d

This is my personal Emacs config. I use this mainly on OS X with GNU Emacs 24.5 and also on Arch
Linux.

This configuration is optimized for *my* use and therefore I wouldn't recommend it as it is to
someone else, but it could however have some small gem that is useful to you.

I decided to experiment with using `use-package` to manage and configure dependencies. Most of the
config is actually made of `use-package` blocks. This has the advantage of making configuration of
packages consistent and also performant since most dependencies are lazy loaded.

Additionally, I wanted to make my setup as simple as possible and therefore decided to try having
all my config in just one file. In order to get around the file I use `outshine` to fold sections
like in `org-mode`. This is what it looks like:

![outshine init](https://dl.dropboxusercontent.com/u/7018537/emacs-init.gif)

## Keybindings

These are my personal keybindings as reported by `use-package`'s function `describe-personal-keybindings`.

Key name    | Command                                             | Comments
----------- | --------------------------------------------------- | -------------------------------
**global**  |                                                     |
`<f5>`      | `my-toggle-fullscreen`                              |
`<f6>`      | `org-agenda`                                        |
`<f7>`      | `org-clock-goto`                                    |
`<f8>`      | `org-clock-in`                                      |
`<f9>`      | `org-pomodoro`                                      |
            |                                                     |
`C-a`       | `my-move-beginning-of-line`                         | was `move-beginning-of-line`
`C-ç`       | `hippie-expand`                                     |
            |                                                     |
`C-c D`     | `my-delete-buffer-and-file`                         |
`C-c a`     | `org-agenda`                                        |
`C-c b`     | `org-iswitchb`                                      |
`C-c c`     | `org-capture`                                       |
`C-c l`     | `org-store-link`                                    |
`C-c r`     | `my-rename-buffer-and-file`                         |
            |                                                     |
`C-c C-m`   | `execute-extended-command`                          |
            |                                                     |
`C-x b`     | `helm-mini`                                         | was `switch-to-buffer`
`C-x g`     | `magit-status`                                      |
`C-x ç`     | `my-comment-or-uncomment-region-or-line`            |
            |                                                     |
`C-x C-b`   | `helm-buffers-list`                                 | was `list-buffers`
`C-x C-f`   | `helm-find-files`                                   | was `find-file`
`C-x C-m`   | `helm-M-x`                                          | was `#<keymap>`
            |                                                     |
`M-/`       | `hippie-expand`                                     | was `dabbrev-expand`
`M-2`       | `my-insert-at-sign`                                 |
`M-3`       | `my-insert-euro-sign`                               |
`M-x`       | `helm-M-x`                                          | was `execute-extended-command`
`M-y`       | `helm-show-kill-ring`                               | was `yank-pop`
            |                                                     |
`s-a`       | `helm-projectile-ag`                                |
`s-f`       | `helm-projectile-find-file`                         |
`s-l`       | `goto-line`                                         |
**emacs-lisp-mode-map** |                                         |
`C-c e`     | `macrostep-expand`                                  |
**window-numbering-keymap** |                                     |
`s-0`       | `select-window-0`                                   |
`s-1`       | `select-window-1`                                   |
`s-2`       | `select-window-2`                                   |
`s-3`       | `select-window-3`                                   |
`s-4`       | `select-window-4`                                   |
`s-5`       | `select-window-5`                                   |
`s-6`       | `select-window-6`                                   |
`s-7`       | `select-window-7`                                   |
`s-8`       | `select-window-8`                                   |
`s-9`       | `select-window-9`                                   |


## Credits

I have used Bozhidar Batsov's config for a long time and this config borrows a lot of knowledge (and
code) from Prelude.
