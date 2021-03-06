treetop-mode.el
===============

An Emacs Lisp mode for Treetop grammar files.

### Notes

Place `treetop-mode.el` on Emacs' load-path, or update your `.emacs` with e.g. `(add-to-list 'load-path "~/.emacs.d/treetop")` to specify the directory where it is located. Add `(require 'treetop-mode)` to `.emacs`. Emacs will enter Treetop mode when visiting a file with the _.tt_ extension. This has only been tested with GNU Emacs 24.

Work is needed to correctly format blocks of Ruby code embedded in grammars.

### Thanks

Thanks to the authors of the [EmacsWiki](http://www.emacswiki.org) pages [here](http://emacswiki.org/emacs/ModeTutorial) and [here](http://www.emacswiki.org/emacs/DerivedMode), and to the folks behind [Treetop](http://cjheath.github.io/treetop/).
