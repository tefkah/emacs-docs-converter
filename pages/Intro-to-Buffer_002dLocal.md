<!-- This is the GNU Emacs Lisp Reference Manual
corresponding to Emacs version 27.2.

Copyright (C) 1990-1996, 1998-2021 Free Software Foundation,
Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being "GNU General Public License," with the
Front-Cover Texts being "A GNU Manual," and with the Back-Cover
Texts as in (a) below.  A copy of the license is included in the
section entitled "GNU Free Documentation License."

(a) The FSF's Back-Cover Text is: "You have the freedom to copy and
modify this GNU manual.  Buying copies from the FSF supports it in
developing GNU and promoting software freedom." -->

<!-- Created by GNU Texinfo 6.7, http://www.gnu.org/software/texinfo/ -->

Next: [Creating Buffer-Local](Creating-Buffer_002dLocal.html), Up: [Buffer-Local Variables](Buffer_002dLocal-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.11.1 Introduction to Buffer-Local Variables

A buffer-local variable has a buffer-local binding associated with a particular buffer. The binding is in effect when that buffer is current; otherwise, it is not in effect. If you set the variable while a buffer-local binding is in effect, the new value goes in that binding, so its other bindings are unchanged. This means that the change is visible only in the buffer where you made it.

The variable’s ordinary binding, which is not associated with any specific buffer, is called the *default binding*. In most cases, this is the global binding.

A variable can have buffer-local bindings in some buffers but not in other buffers. The default binding is shared by all the buffers that don’t have their own bindings for the variable. (This includes all newly-created buffers.) If you set the variable in a buffer that does not have a buffer-local binding for it, this sets the default binding, so the new value is visible in all the buffers that see the default binding.

The most common use of buffer-local bindings is for major modes to change variables that control the behavior of commands. For example, C mode and Lisp mode both set the variable `paragraph-start` to specify that only blank lines separate paragraphs. They do this by making the variable buffer-local in the buffer that is being put into C mode or Lisp mode, and then setting it to the new value for that mode. See [Major Modes](Major-Modes.html).

The usual way to make a buffer-local binding is with `make-local-variable`, which is what major mode commands typically use. This affects just the current buffer; all other buffers (including those yet to be created) will continue to share the default value unless they are explicitly given their own buffer-local bindings.

A more powerful operation is to mark the variable as *automatically buffer-local* by calling `make-variable-buffer-local`. You can think of this as making the variable local in all buffers, even those yet to be created. More precisely, the effect is that setting the variable automatically makes the variable local to the current buffer if it is not already so. All buffers start out by sharing the default value of the variable as usual, but setting the variable creates a buffer-local binding for the current buffer. The new value is stored in the buffer-local binding, leaving the default binding untouched. This means that the default value cannot be changed with `setq` in any buffer; the only way to change it is with `setq-default`.

**Warning:** When a variable has buffer-local bindings in one or more buffers, `let` rebinds the binding that’s currently in effect. For instance, if the current buffer has a buffer-local value, `let` temporarily rebinds that. If no buffer-local bindings are in effect, `let` rebinds the default value. If inside the `let` you then change to a different current buffer in which a different binding is in effect, you won’t see the `let` binding any more. And if you exit the `let` while still in the other buffer, you won’t see the unbinding occur (though it will occur properly). Here is an example to illustrate:

    (setq foo 'g)
    (set-buffer "a")
    (make-local-variable 'foo)

<!---->

    (setq foo 'a)
    (let ((foo 'temp))
      ;; foo ⇒ 'temp  ; let binding in buffer ‘a’
      (set-buffer "b")
      ;; foo ⇒ 'g     ; the global value since foo is not local in ‘b’
      body…)

<!---->

    foo ⇒ 'g        ; exiting restored the local value in buffer ‘a’,
                     ; but we don’t see that in buffer ‘b’

<!---->

    (set-buffer "a") ; verify the local value was restored
    foo ⇒ 'a

Note that references to `foo` in `body` access the buffer-local binding of buffer ‘`b`’.

When a file specifies local variable values, these become buffer-local values when you visit the file. See [File Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables) in The GNU Emacs Manual.

A buffer-local variable cannot be made terminal-local (see [Multiple Terminals](Multiple-Terminals.html)).

Next: [Creating Buffer-Local](Creating-Buffer_002dLocal.html), Up: [Buffer-Local Variables](Buffer_002dLocal-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
