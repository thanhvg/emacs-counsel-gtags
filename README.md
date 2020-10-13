# counsel-gtags.el [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] [![Build Status](https://travis-ci.org/FelipeLema/emacs-counsel-gtags.svg?branch=master)](https://travis-ci.org/FelipeLema/emacs-counsel-gtags)

[GNU GLOBAL](https://www.gnu.org/software/global/) interface of [ivy](https://github.com/abo-abo/swiper).

## Tasks

- [X] Basic commands
- [X] find file command
- [X] Tag command
- [ ] Context command(dwim)
 - [X] Find definition and references
 - [ ] include header support
- [ ] `GTAGSLIBPATH` support
- [X] Basic History command
- [ ] History navigate command
- [x] Tramp support

## Requirements

- Emacs 24.3 or higher versions
- [counsel](https://github.com/abo-abo/swiper)
- GNU global 6.2.3 or higher versions

## Installation

`counsel-gtags` is available on [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org/)

You can install `counsel-gtags` with the following command.

<kbd>M-x package-install [RET] counsel-gtags [RET]</kbd>

## Basic Usage

You can search for tags or files in the database. Every time you jump to a
definition, reference or symbol the current position is pushed to the context
stack. You can navigate backward and forward within this stack with
`counsel-gtags-go-backward` and `counsel-gtags-go-forward`.

#### counsel-gtags-find-definition

Search for definition.

#### counsel-gtags-find-reference

Search for references.

#### counsel-gtags-find-symbol

Search for symbol references.

#### counsel-gtags-find-file

Search for file among tagged files.

#### counsel-gtags-go-backward

Go to previous position in context stack.

#### counsel-gtags-go-forward

Go to next position in context stack.

#### counsel-gtags-create-tags

Create GNU GLOBAL tags.

#### counsel-gtags-update-tags

Update tags.

#### counsel-gtags-dwim

Find name by context.

- Jump to tag definition if cursor is on tag reference
- Jump to tag reference if cursor is on tag definition

## Sample Configuration

```lisp
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))
```

There are some useful custom variables like

- counsel-gtags-simule-xref: To replace the xref bindings with some of our functions.
- counsel-gtags-use-input-at-point: To use `thing-at-point' functions.
- counsel-gtags-debug-mode : To enable debug mode which prints some of
  the commands in the \*Messages\* buffer.

## See also

My other "using global from withing Emacs" package: [global-tags.el](https://launchpad.net/global-tags.el)
I stopped using this package (, created) and started using that one, which works best with [ivy-xref](https://github.com/alexmurray/ivy-xref). This package and [global-gtags](https://github.com/leoliu/ggtags) are really hard to maintain because newcomers have to absorb a lot of hacks so the code can work.

This packages is mostly "make things friendly for ivy", while `global-gtags` is mostly "fight compilation-mode to present a decent UI". None use Emacs' native `xref.el`.

[melpa-link]: https://melpa.org/#/counsel-gtags
[melpa-stable-link]: https://stable.melpa.org/#/counsel-gtags
[melpa-badge]: https://melpa.org/packages/counsel-gtags-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/counsel-gtags-badge.svg
