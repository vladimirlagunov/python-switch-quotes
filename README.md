# python-switch-quotes

[![Travis-CI build status](https://api.travis-ci.org/werehuman/python-switch-quotes.svg?branch=master)](https://travis-ci.org/werehuman/python-switch-quotes)

Converts strings like `'this'` to strings like `"this"`.
Supports raw strings, docstrings and strings with escaped quotes.

![Example](http://i.imgur.com/xvjsbbs.gif)

# Installation

You can install it from MELPA:
* http://melpa.org/#/getting-started
* `M-x package-install RET python-switch-quotes RET`

Or clone this repository, open `python-switch-quotes.el` in Emacs and run `M-x package-install-from-buffer RET`.

Then put into your `init.el`:

```emacs
(require 'python-switch-quotes)
(define-key python-mode-map (kbd "C-c '") 'python-switch-quotes)
```

# Usage

Move cursor to the middle of string and press `C-c '`.
