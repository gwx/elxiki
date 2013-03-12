(defmenu _init 
  (format "
> Filtering is: %s
+ turn filtering %s
| Immediately after doing `elxiki-command', you may filter
| results by typing. Try it on the list of keys below:
+ Filtering Keys
  | Stop filtering results: C-g, <return>
  | Perform another action: C-<return>
  | Perform action and hide siblings: <tab>
  | Perform action and replace parent: C-/
+ Cursor
  | While filtering, your cursor will change to a box.
  | You can change this with the variable:
  ! elxiki-filter-cursor-type
| By default, M-<return> is bound to 
| `elxiki-command-switch-filter', which is like 
| `elxiki-command' but it negates the filter value.
"
          (if elxiki-filter-inhibit "Off" "On")
          (if elxiki-filter-inhibit "on" "off")))

(defmenu turnfilteringoff
  (setq elxiki-filter-inhibit t)
  "| Filtering turned off.")

(defmenu turnfilteringon
  (setq elxiki-filter-inhibit nil)
  "| Filtering turned on.")
