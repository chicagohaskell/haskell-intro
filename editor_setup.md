---
title: Haskell Editor Setup
author: Matt Wraith
---

![Haskell Editor Survey Results](https://taylor.fausak.me/static/images/2018/11/18/question-043.svg)

We're going to do the top 4.

1. Emacs
2. Vim
3. VSCode
4. Atom

[Haskell IDE Comparison Chart](https://github.com/rainbyte/haskell-ide-chart)

## Install stack

[https://docs.haskellstack.org/en/stable/install_and_upgrade/](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
```

or

```shell
$ apt/brew install haskell-stack
$ pacman -S stack
```

then

```shell
$ stack upgrade
```

You may need to add `$HOME/.local/bin` to your `PATH`:

```shell
$ export PATH=$HOME/.local/bin:$PATH
```

## ghcid

Everybody can use ghcid. It's a fantastic tool that compiles in the terminal.

```shell
$ stack install ghcid
```

How to use:

```shell
$ cd /path/to/your/stack/project
$ ghcid
```

## Emacs

Intero gives you a lot of power. Flychecking code. Querying types of subexpressions:
```elisp
(package-install 'haskell-mode)
(package-install 'intero)

(require 'haskell)
(require 'haskell-mode)
(require 'intero)

(intero-global-mode 1)

;; If you want to set indentation for spaces
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(setq haskell-indentation-layout-offset 4
      haskell-indentation-left-offset 4)
```

## Vim

Ale works well with newest vim:
```vimscript
Plug 'w0rp/ale'
...
let g:ale_set_quickfix = 1
let g:ale_linters = {
\   'haskell': ['stack-ghc'],
\}
```

If you use neovim, you can also get intero and ghcid plugins:
```vimscript
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'parsonsmatt/intero-neovim'
...
let g:intero_use_neomake = 0 " neomake seems to conflict with ale
```

For more info on intero-neovim: [https://github.com/parsonsmatt/intero-neovim](https://github.com/parsonsmatt/intero-neovim)

## VSCode

Install [https://marketplace.visualstudio.com/items?itemName=Vans.haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)

For all projects that use Haskero, you must:
```shell
$ cd /path/to/your/stack/project
$ stack build intero
```

## Atom

```shell
$ apm install atom-haskell
```

[https://atom-haskell.github.io/](https://atom-haskell.github.io/)

or for a more minimal install:

```shell
apm install language-haskell ide-haskell ide-haskell-cabal ide-haskell-repl
```

## General Advice

- You probably want stack, but it's possible to use cabal or nix or the Haskell Platform.
- Everybody can use ghcid. It's very fast and simple.
- ghc-mod only works well for small projects. It will bog down your editor if you're not careful. This is maybe a
  problem with the Atom plugin.

## Summary

- Emacs  -> [haskell-mode][hmode] + [intero][intero]
- vim    -> [ale][ale] with stack-ghc linter
- neovim -> [ghcid][ghcid] + [intero][intero-nv]
- VSCode -> [Haskero][haskero] ([docs][haskero-docs])
- Atom   -> [atom-haskell][ah-site] (simpler subset of atom-haskell: ide-haskell + ide-haskell-cabal)


[hmode]: http://haskell.github.io/haskell-mode/
[intero]: https://haskell-lang.org/intero
[intero-nv]: https://github.com/parsonsmatt/intero-neovim
[ghcid]: https://github.com/ndmitchell/ghcid
[haskero]: https://marketplace.visualstudio.com/items?itemName=Vans.haskero
[haskero-docs]: https://gitlab.com/vannnns/haskero/blob/master/client/doc/installation.md
[ale]: https://github.com/w0rp/ale
[ah-site]: https://atom-haskell.github.io/
[ah]: https://atom.io/packages/atom-haskell
