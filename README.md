# Twauctex

This mode integrates some tweaks into AUCtex that makes it easier to
write LaTeX. More precisely, it does the following:

* Enable one-sentence-per-line mode.
* Supresses unescaped underscores and ampersands.
* Adds a command to compress and align tables (Bound to `C-x f` by
default).
* Adds intersentence space when a sentence ends with capital letters.

## Features

### One-sentence-per line mode

The biggest feature of twauctex is the one-sentence-per-line mode.
One-sentence-per-line (OSPL) makes it much easier to read the diffs
generated from edits. This is especially useful when collaborating on
a LaTeX document (and everyone uses OSPL).

This works by changing the fill function to work on sentences, and
making sentence-ending characters electric to insert a line break when
required. I've taken care to not make this too intrusive –
`twauctex-electric-environments` and
`twauctex-inhibited-electric-macros` allow you to disable the electric
functionality in certain macros, and enable them only in certain
environments.

Additionally, twauctex has a feature to not break when entering common
abbreviations (such as M.Sc.).

### Escaping ampersands and underscores

Ampersands and underscores are somewhat annoying in LaTeX, because
they can only be entered without escape characters in certain macros.
I've added functionality to autoescape these characters when not in a
macro where they have special meaning (such as `matrix` or `align`).

### Compressing and aligning tables

It's very annoying to manually align tables, and remembering the
correct syntax for `align` is a chore. Thus, I've added two commands
that make aligning tables a bit easier. Bound to `C-c f` by default,
it will align all the `&`s. If passed an argument via `C-u C-c f`, the
table will first be compressed by removing all spaces before column
separators, and then realign the table.

### Intersentence space where required.

As `chktex` is fond of reminding you, if you end a sentence with
capital letters, TeX might think you're using an abbreviation, and
might not space the sentence correctly. twauctex tries to rectify this
by checking for this case (with all the bells and whistles mentioned
above) and automatically inserting the `\@` when required.

## Installation

```elisp
(add-to-list 'load-path "D:\\repos\\twauctex\\")
(require 'twauctex)
(twauctex-global-mode)
```

There's some dependencies: Auctex (obviously) and
`visual-fill-column-mode`. Strictly speaking,
`visual-fill-column-mode` is not required to use one-sentence-per-line
mode, but reading sentences that go all the way across your screen
could be difficult. For now, it's a hard requirement.

## Usage

Simply write your latex as you would normally. twauctex will take care
of breaking the line whenever you enter a sentence-ending character.
If the standard settings do not agree with you, use `(customize-group
'twauctex)`, and you can customize the builtin configuration.

