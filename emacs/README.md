# jml's emacs configuration

## How to use

First, [install Cask](https://github.com/cask/cask).

Then,

```
$ git clone https://github.com/jml/emacs-configuration.git
$ rm -rf ~/.emacs.d
$ ln -s emacs-configuration ~/.emacs.d
$ cd ~/.emacs.d
$ cask install
```

## Things to look into

* elpy / pyvenv
* company
* YouCompleteMe for Emacs
* popwin
* make yasnippet config much better
* more structured way of OS-specific & site-specific configuration
* markdown-mode that's based on [CommonMark](http://commonmark.org/)
* Python flycheck isn't as check-y as it might be
* github gist support
* explore richer helm configuration
  [washort](https://github.com/washort/dotfiles/blob/master/emacs.d/emacs-init.org#helm)

[washort's emacs config](https://github.com/washort/dotfiles/blob/master/emacs.d/emacs-init.org)
is a source of inspiration.
