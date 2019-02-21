# dotfiles

jml's configuration, not including
[emacs configuration](https://github.com/jml/emacs-configuration).

## Howto

### macOS

#### Dependencies

All of these are optional, but highly recommended.

- [brew](https://brew.sh/)
- [direnv](https://direnv.net/)
- [pyenv](https://github.com/pyenv/pyenv)
- `coreutils` - `brew install coreutils`
- `git` - `brew install git`

##### bash

```console
$ brew install bash
$ sudo vi /etc/shells  # Add /usr/local/bin/bash to shells
# chsh -s /usr/local/bin/bash
```

#### Install shell files

```console
$ mkdir -p src
$ cd src
$ git clone git@github.com:jml/dotfiles.git
$ cat > ~/.bash_profile
. $HOME/src/dotfiles/bash/bashrc
```
