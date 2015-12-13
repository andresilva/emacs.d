# emacs.d

This is my personal Emacs config. I use this mainly on OS X with Yamamoto's Emacs mac port and also
on Arch Linux.

This configuration is optimized for *my* use and therefore I wouldn't recommend it as it is to
someone else, but it could however have some small gem that is useful to you.

I decided to experiment with using `use-package` to manage and configure dependencies. Most of the
config is actually made of `use-package` blocks. This has the advantage of making configuration of
packages consistent and also performant since most dependencies are lazy loaded.

Additionally, I wanted to make my setup as simple as possible and therefore decided to try having
all my config in just one file. In order to get around the file I use `outshine` to fold sections
like in `org-mode`.

## Credits

I have used Bozhidar Batsov's config for a long time and this config borrows a lot of knowledge (and
code) from Prelude.
