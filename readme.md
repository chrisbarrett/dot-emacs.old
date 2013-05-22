# dot-emacs

This is my Emacs configuration. It is obviously very personalized - my key bindings are designed around [Evil-mode](http://gitorious.org/evil/pages/Home).

## Installation

Requires GNU Make, curl and Git.

```bash
cd
mv .emacs.d .emacs.d.backup
git clone git@github.com:chrisbarrett/dot-emacs.git .emacs.d
cd .emacs.d
make
```
You can install additional features with `make all`, or using their individual make tasks (eg `make ruby`).

## Features

Since Evil-mode frees up the Meta key, I use M-*key* for helm and dired commands. Normal Emacs motion commands will not work!

### Language Config

Sane configurations are provided for several languages and environments:

* Ruby/Rails
* Python
* Elisp
* Clojure
* Scheme
* Haskell
* SuperCollider

### Simple key-bindings

Common commands, such as helm or w3m commands, have single-stroke key-bindings:

* `C-j` - helm-projectile
* `M-j` - helm-mini
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

### Modal Windows

Certain commands show buffers in a *modal* manner; they expand to fill the frame and restore the previous state when toggled or killed. This behaviour is provided for:

* most magit commands, including:
  * `M-S` - magit-status
  * `C-x g d` - magit-diff
  * `C-x g l` - magit-log
  * `C-x g s` - magit-show
  * `C-x g r` - magit-reflog
* ansi-term - `M-T`
* w3m - `M-W`
* ido notes buffer - `M-O`
* init.el - `M-I`

### Misc

* I try to keep the startup time under 2 seconds on my machine.

* Configured to use [solarized](https://github.com/bbatsov/solarized-emacs)  and a modified version of [ir-black](https://github.com/jmdeldin/ir-black-theme.el):
    * `M-x solarized-dark`
    * `M-x solarized-light`
    * `M-x ir-black`

* The mode-line is customised to remove clutter. The more ubiquitous minor-modes are hidden.
