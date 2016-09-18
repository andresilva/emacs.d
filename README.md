# emacs.d

[![Build Status](https://img.shields.io/travis/andrebeat/emacs.d/master.svg)](https://travis-ci.org/andrebeat/emacs.d)

This is my personal Emacs config. I use this mainly on OS X with GNU Emacs 25.1 and also on Arch
Linux.

This configuration is optimized for *my* use and therefore I wouldn't recommend it as it is to
someone else, but it could however have some small gem that is useful to you.

I decided to experiment with using [`use-package`][use-package] to manage and configure
dependencies. Most of the config is actually made of [`use-package`][use-package] blocks. This has
the advantage of making configuration of packages consistent and also performant since most
dependencies are lazy loaded.

Additionally, I wanted to make my setup as simple as possible and therefore decided to try having
all my config in just one file. In order to get around the file I use `outline-minor-mode` with
[`outshine`](https://github.com/tj64/outshine) to fold sections like in `org-mode`. This
is what it looks like:

![outshine init](https://dl.dropboxusercontent.com/u/7018537/emacs-init.gif)

I've also started using [`evil-mode`][evil-mode] for "vim-like" editing coupled with
[`evil-leader`](https://github.com/cofi/evil-leader) for ergonomic keybindings.

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
`C-q`       | `evil-execute-macro`                                | was `quoted-insert`
`C-s`       | `swiper`                                            | was `isearch-forward`
`C-{`       | `er/expand-region`                                  |
`C-ç`       | `hippie-expand`                                     |
            |                                                     |
`C-c 2`     | `my-layout-double-columns`                          |
`C-c 3`     | `my-layout-triple-columns`                          |
`C-c D`     | `crux-delete-file-and-buffer`                       |
`C-c a`     | `org-agenda`                                        |
`C-c b`     | `org-iswitchb`                                      |
`C-c c`     | `org-capture`                                       |
`C-c f`     | `my-toggle-window-focus`                            |
`C-c g`     | `counsel-git`                                       |
`C-c j`     | `counsel-git-grep`                                  |
`C-c k`     | `counsel-ag`                                        |
`C-c l`     | `org-store-link`                                    |
`C-c n`     | `crux-cleanup-buffer-or-region`                     |
`C-c r`     | `crux-rename-buffer-and-file`                       |
            |                                                     |
`C-c C-m`   | `smex`                                              |
`C-c C-r`   | `ivy-resume`                                        |
            |                                                     |
`C-x 2`     | `my-split-window-below-and-focus`                   | was `split-window-below`
`C-x 3`     | `my-split-window-right-and-focus`                   | was `split-window-right`
`C-x b`     | `ivy-switch-buffer`                                 | was `switch-to-buffer`
`C-x g`     | `magit-status`                                      |
`C-x y`     | `yas-describe-tables`                               |
`C-x ç`     | `my-comment-or-uncomment-region-or-line`            |
            |                                                     |
`C-x C-f`   | `counsel-find-files`                                | was `find-file`
`C-x C-m`   | `counsel-M-x`                                       | was `#<keymap>`
            |                                                     |
`M-/`       | `hippie-expand`                                     | was `dabbrev-expand`
`M-x`       | `counsel-M-x`                                       | was `execute-extended-command`
`M-y`       | `counsel-yank-pop`                                  | was `yank-pop`
            |                                                     |
`s-b`       | `ivy-switch-buffer`                                 |
`s-k`       | `crux-kill-whole-line`                              | was `kill-this-buffer`
`s-l`       | `goto-line`                                         |
**emacs-lisp-mode-map** |                                         |
`C-c e`     | `macrostep-expand`                                  |
`C-c C-e`   | `eval-region`                                       |
**ivy-minibuffer-map** |                                          |
`<escape>`  | `minibuffer-keyboard-quit`                          |
`<tab>`     | `ivy-alt-done`                                      | was `ivy-partial-or-done`
            |                                                     |
`C-j`       | `ivy-next-line`                                     | was `ivy-alt-done`
`C-k`       | `ivy-previous-line`                                 |
**override-global-map** |
`C-c p b`   | `counsel-projectile-switch-to-buffer`
`C-c p f`   | `counsel-projectile-find-file`
`C-c p p`   | `counsel-projectile`
`C-c p s s` | `counsel-projectile-ag`
            |
`s-a`       | `counsel-projectile-ag`
`s-p b`     | `counsel-projectile-switch-to-buffer`
`s-p f`     | `counsel-projectile-find-file`
`s-p p`     | `counsel-projectile`
`s-p s s`   | `counsel-projectile-ag`
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
`SPC b`     | `ivy-switch-buffer`
`SPC f`     | `counsel-find-files`
`SPC g`     | `magit-status`
`SPC v`     | `expand-region`
            |
`SPC ç`     | `evilnc-comment-or-uncomment-lines`
`SPC ci`    | `evilnc-comment-or-uncomment-lines`
`SPC cl`    | `evilnc-quick-comment-or-uncomment-to-the-line`
`SPC cc`    | `evilnc-copy-and-comment-lines`
`SPC cp`    | `evilnc-comment-or-uncomment-paragraphs`
`SPC cr`    | `comment-or-uncomment-region`
`SPC cv`    | `evilnc-toggle-invert-comment-line-by-line`
            |
`SPC p p`   | `counsel-projectile`
`SPC p f`   | `counsel-projectile-find-file`
`SPC p a`   | `counsel-projectile-ag`
            |
`SPC j j`   | `avy-goto-char`
`SPC j w`   | `avy-goto-word-or-subword-1`
`SPC j l`   | `avy-goto-line`
`SPC j b`   | `avy-pop-mark`
**scala-mode-map** |
`SPC .`     | `ensime-edit-definition`
`SPC p c`   | `ensime-sbt-do-compile`
**rust-mode-map** |
`SPC .`     | `racer-find-definition`

Since [`evil-mode`][evil-mode] relies on `Esc` to switch back to normal state, I have set up my `Caps
Lock` key to act as `Esc` when tapped and as `Ctrl` when pressed.

## Credits

I have used Bozhidar Batsov's config for a long time and this config borrows a lot of knowledge (and
some code) from [Prelude](https://github.com/bbatsov/prelude). Additionally, I've also found some of
[Spacemacs](https://github.com/syl20bnr/spacemacs)'s layers to be very useful.

[use-package]: https://github.com/jwiegley/use-package
[evil-mode]: https://bitbucket.org/lyro/evil/wiki/Home
