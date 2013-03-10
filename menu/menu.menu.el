(defmenu _init "
+ What is a Menu?
  | Elxiki menus are menus you type into a buffer, and then
  | open with a call to `elxiki-command', bound to C-<return>
  | and M-<return> by default. Menus can expand to sub-menu
  | items, as well as run arbitrary emacs lisp code.
|
+ Defining Menus
  | To define a menu, type the name of one and then call `elxiki-menu-edit',
  | bound to C-c [ by default. This will find the default file for that menu.
  |
  | There are two ways to define a menu:
  | - In a .menu file, which is just text representing the menu.
  |   The elxiki menu is one of these:
  @ elxiki
  | - In a .menu.el file, which lets the menu run emacs lisp code.
  |   The theme menu is a good example:
  @ theme
  |   You may also want to take a look at the documentation
  |   for defmenu:
  !! (describe-function 'defmenu)
+ Using Menus
  | Menus are explicitly declared with an @ prefix. This can
  | usually be skipped, unless you want to link from one menu
  | to another.
|
> Here are all the defined menus:
@ all/
")
