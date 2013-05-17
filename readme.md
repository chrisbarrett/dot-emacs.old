# dot-emacs

This is my Emacs configuration. It is obviously very personalized - my key bindings are designed around [Evil-mode](http://gitorious.org/evil/pages/Home).

# Installation

Requires GNU Make, curl and Git.

```bash
cd
mv .emacs.d .emacs.d.backup
git clone git@github.com:chrisbarrett/dot-emacs.git .emacs.d
cd .emacs.d
make
```
Additional features require specific language runtimes to be installed. You can install these features with `make all`, or using their individual make tasks (eg `make ruby`).

# Notes

* Since Evil-mode frees up the Meta key, I use M-*key* for helm and dired commands. Normal Emacs motion commands will not work!
    * `M-t` - toggle terminal between various window configurations
    * `M-d` - shows current file in dired
    * `M-g` - magit-status
    * `C-j` - helm-projectile
    * `M-j` - helm-mini
    * `M-e` - goe to URL
    * `M-w` - w3m bookmarks
    * `M-i` - helm-imenu
    * `M-s` - Google search
    * `M-a` - helm-apropos
    * `M-b` - buffers list with helm
    * `M-m` - man page with helm
    * `M-r` - rename symbol at point

    etc...

* I run Emacs as a daemon, though I try to keep the startup time under 2 seconds on my machine

* Configured to use [solarized](https://github.com/bbatsov/solarized-emacs)  and a modified version of [ir-black](https://github.com/jmdeldin/ir-black-theme.el):
    * `M-x solarized-dark`
    * `M-x solarized-light`
    * `M-x ir-black`

* The mode-line is customised to remove clutter and show Git status at-a-glance. The more ubiquitous minor-modes are hidden.

* Configurations for several languages and environments:
    * Ruby/Rails
    * Python
    * Elisp
    * Clojure
    * Scheme
    * Haskell
    * SuperCollider
