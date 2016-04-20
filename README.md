# emacs.d

[![Build Status](https://img.shields.io/travis/andrebeat/emacs.d/master.svg)](https://travis-ci.org/andrebeat/emacs.d)

This is my personal Emacs config. I use this mainly on OS X with GNU Emacs 24.5 and also on Arch
Linux.

This configuration is optimized for *my* use and therefore I wouldn't recommend it as it is to
someone else, but it could however have some small gem that is useful to you.

I decided to experiment with using [`use-package`][use-package] to manage and configure
dependencies. Most of the config is actually made of [`use-package`][use-package] blocks. This has
the advantage of making configuration of packages consistent and also performant since most
dependencies are lazy loaded.

Additionally, I wanted to make my setup as simple as possible and therefore decided to try having
all my config in just one file. In order to get around the file I use `outline-minor-mode` with
[`outline-magic`](https://github.com/tj64/outline-magic) to fold sections like in `org-mode`. This
is what it looks like:

![outshine init](https://dl.dropboxusercontent.com/u/7018537/emacs-init.gif)

I've also started using [`evil-mode`](https://bitbucket.org/lyro/evil/wiki/Home) for "vim-like"
editing coupled with [`evil-leader`](https://github.com/cofi/evil-leader) for ergonomic keybindings.

## Keybindings

These are my personal keybindings as reported by [`use-package`][use-package]'s function
`describe-personal-keybindings`.

Key binding | Command                                             | Comments
----------- | --------------------------------------------------- | -------------------------------
**global**  |                                                     |
`<f5>`      | `my-toggle-frame-fullscreen`                        |
`<f6>`      | `org-agenda`                                        |
`<f7>`      | `org-clock-goto`                                    |
`<f8>`      | `org-clock-in`                                      |
`<f9>`      | `org-pomodoro`                                      |
            |                                                     |
`C-a`       | `crux-move-beginning-of-line`                       | was `move-beginning-of-line`
`C-{`       | `er/expand-region`                                  |
`C-ç`       | `hippie-expand`                                     |
            |                                                     |
`C-c D`     | `crux-delete-file-and-buffer`                       |
`C-c a`     | `org-agenda`                                        |
`C-c b`     | `org-iswitchb`                                      |
`C-c c`     | `org-capture`                                       |
`C-c l`     | `org-store-link`                                    |
`C-c n`     | `crux-cleanup-buffer-or-region`                     |
`C-c r`     | `crux-rename-buffer-and-file`                       |
            |                                                     |
`C-c C-m`   | `execute-extended-command`                          |
            |                                                     |
`C-x b`     | `helm-mini`                                         | was `switch-to-buffer`
`C-x g`     | `magit-status`                                      |
`C-x y`     | `yas-describe-tables`                               |
`C-x ç`     | `my-comment-or-uncomment-region-or-line`            |
            |                                                     |
`C-x C-b`   | `helm-buffers-list`                                 | was `list-buffers`
`C-x C-f`   | `helm-find-files`                                   | was `find-file`
`C-x C-m`   | `helm-M-x`                                          | was `#<keymap>`
            |                                                     |
`M-/`       | `hippie-expand`                                     | was `dabbrev-expand`
`M-2`       | `my-insert-at-sign`                                 | was `digit-argument`
`M-3`       | `my-insert-euro-sign`                               | was `digit-argument`
`M-x`       | `helm-M-x`                                          | was `execute-extended-command`
`M-y`       | `helm-show-kill-ring`                               | was `yank-pop`
            |                                                     |
`s-a`       | `helm-projectile-ag`                                | was `mark-whole-buffer`
`s-f`       | `helm-projectile-find-file`                         | was `isearch-forward`
`s-k`       | `crux-kill-whole-line`                              | was `kill-this-buffer`
`s-l`       | `goto-line`                                         |
**emacs-lisp-mode-map** |                                         |
`C-c e`     | `macrostep-expand`                                  |
`C-c C-e`   | `eval-region`                                       |
**helm-map** |                                                    |
`<tab>`     | `helm-execute-persistent-action`                    |
            |                                                     |
`C-i`       | `helm-execute-persistent-action`                    | was `helm-select-action`
`C-z`       | `helm-select-action`                                | was `helm-execute-persistent-action`
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

### Evil Leader

I set my leader key to `SPC` since it's pressed with the thumb.

Key binding | Command
----------- | -------------------------------------
**global**  |
`SPC s`     | `save-buffer`
`SPC k`     | `kill-this-buffer`
`SPC d`     | `delete-window`
`SPC b`     | `helm-mini`
`SPC f`     | `helm-find-files`
`SPC g`     | `magit-status`
            |
`SPC p p`   | `helm-projectile-switch-project`
`SPC p f`   | `helm-projectile-find-file`
`SPC p a`   | `helm-projectile-ag`
**scala-mode-map** |
`SPC .`     | `ensime-edit-definition`
`SPC p c`   | `ensime-sbt-do-compile`

### Key chord

I use [key chord](https://github.com/emacsmirror/key-chord) to define two chords to toggle between
`evil`'s normal and insert state.

Key binding | Command
----------- | -------------------------------------
**global**  |
`jk`        | `my-toggle-evil-state`
`jf`        | `my-toggle-evil-state`

## Credits

I have used Bozhidar Batsov's config for a long time and this config borrows a lot of knowledge (and
some code) from [Prelude](https://github.com/bbatsov/prelude). Additionally, I've also found some of
[Spacemacs](https://github.com/syl20bnr/spacemacs)'s layers to be very useful.

[use-package]: https://github.com/jwiegley/use-package
