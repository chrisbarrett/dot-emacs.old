# dot-emacs

This is my Emacs configuration. It is obviously very personalized - my key
bindings are designed around
[Evil-mode](http://gitorious.org/evil/pages/Home), though they can be
easily disabled.

## Installation

Requires GNU Make, curl and Git.

```bash
cd
mv .emacs.d .emacs.d.backup
git clone git@github.com:chrisbarrett/dot-emacs.git .emacs.d
cd .emacs.d
make
```
You can install additional features with `make all`, or using their individual
make tasks (eg `make ruby`). I pretty much only test on my machine, so YMMV.

## Features

I use Evil-mode for Vim emulation. Since Evil-mode frees up the Meta key, I
use M-<key>key</key> for helm and dired commands.

You can restore Emacs' normal key bindings by setting
`cb:use-vim-keybindings?` to nil in `init.el`.

### Language Config

Sane configurations are provided for several languages and environments:

* Ruby/Rails
* Haskell
* C
* Python
* Elisp
* Clojure
* Scheme
* SuperCollider

### Simple key-bindings

Common commands, such as helm or w3m commands, have single-stroke key-bindings:

* `C-j` - helm-projectile
* `M-h` - helm-mini
* `M-r` - rename symbol at point (in defun)
* `M-R` - rename symbol at point  (in whole buffer)
* `M-d` - shows current file in dired
* `M-o` - org-capture
* `M-e` - go to URL
* `M-w` - w3m bookmarks
* `M-i` - helm-imenu
* `M-s` - Google search
* `M-a` - helm-apropos
* `M-b` - buffers list with helm
* `M-m` - man page with helm
* `M-I` - find lisp config file with ido

### Modal Windows

Certain commands show buffers in a *modal* manner; they expand to fill the frame and restore the previous state when toggled or killed. This behaviour is provided for:

* most magit commands, including:
  * `M-G` - magit-status
  * `C-x g d` - magit-diff
  * `C-x g l` - magit-log
  * `C-x g s` - magit-show
  * `C-x g r` - magit-reflog
* ansi-term - `M-T`
* w3m - `M-W`
* org-agenda - `M-O`

### Misc

* I try to keep the startup time under 2 seconds on my machine.

* Configured to use [solarized](https://github.com/bbatsov/solarized-emacs)  and a modified version of [ir-black](https://github.com/jmdeldin/ir-black-theme.el):
    * `M-x solarized-dark` or `M-x dark`
    * `M-x solarized-light` or `M-x light`
    * `M-x ir-black` or `M-x black`

* The selected colour theme is saved between sessions

* The mode-line is customised to remove clutter. The more ubiquitous minor-modes are hidden.
