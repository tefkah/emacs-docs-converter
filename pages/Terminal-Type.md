

Next: [Window Configuration Type](Window-Configuration-Type.html), Previous: [Frame Type](Frame-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.5.5 Terminal Type

A *terminal* is a device capable of displaying one or more Emacs frames (see [Frame Type](Frame-Type.html)).

Terminals have no read syntax. They print in hash notation giving the terminal’s ordinal number and its TTY device file name.

```lisp
(get-device-terminal nil)
     ⇒ #<terminal 1 on /dev/tty>
```
