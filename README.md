ElXiKi
======

Elxiki is a emacs minor mode that performs context-aware actions based
on the line at point. For instance, on the line "+ /home/", it will
list all files in the home directory below it. Activating one of those
lines will find that file. Other examples are "google/emacs/" to bring
up a google search, or "env/" to list all your environment variables,
which can then be edited directly.

Inspired by [Xiki](http://xiki.org), but this time implemented in elisp.

Xiki is really nice, but it (a) has a ton of extra dependencies, and
(b) overwrites a lot of emacs's defaults. Elxiki is an attempt writing
at some of the easier to implement features in emacs lisp while
following (more of) the emacs style guidelines.

Features
--------

At point, you can:

* Fold/Unfold a directory.
  * Works with tramp too.
* Run synchronous and asynchronous shell commands.
  * Also in subdirectories.
* Run elisp.
* Open files.
* Open menus.
  * Have menu items run arbitrary code.
* Filter output by typing.

Installation
------------

Just require elxiki:

    (add-to-list 'load-path "the/elxiki/folder/")
    (require 'elxiki)

To get started, open up a buffer (*scratch* works fine), run 
`M-x elxiki-mode`, goto a new line, type `elxiki/`, and hit
`C-<return>`.

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

#### Menus:

"@ " or prefixless lines will open up menus, if you have them
defined. The all/ menu lists all menus.

Stuff To Add
------------

* Saving menus.
* Work on windows, etc.
* Comment prefixes.