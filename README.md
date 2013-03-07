ElXiKi
======

A pale mimicry of [Xiki](http://xiki.org) implemented in elisp.

Xiki is really nice, but it (a) has a ton of extra dependencies, and
(b) overwrites a lot of emacs's defaults. Here's my attempt writing at
some of the easier to implement features in emacs lisp.

This is nowhere near being done.

Features
--------

Right now there's only really basic stuff.

At point, you can:

* Fold/Unfold a directory.
* Run synchronous and asynchronous commands.
  * Also in subdirectories.
* Run elisp.
* Open files.


Installation
------------

Just require elxiki:

    (add-to-list 'load-path "the/elxiki/folder/")
    (require 'elxiki)

Usage
-----

Just `M-x elxiki-mode`, and then use `elxiki-command` (bound by
default to `C-<return>` and `M-<return>`) to do things.

#### Open a directory:

Type a directory name on a new line starting with "~", ".", or "/" and
ending with "/". Then you can open/close it with `elxiki-command`.

#### Run a shell command:

Any line starting with "$ " will run as a shell command inline. "% "
Can be used for asynchronous commands. If the lines are indented below
a directory, they are run as if they were in that directory.

#### Run emacs lisp:
"! " lines will run emacs lisp commands inline.

Stuff To Add
------------

* Menus
