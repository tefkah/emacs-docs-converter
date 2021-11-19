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

Next: [Equality Predicates](Equality-Predicates.html), Previous: [Circular Objects](Circular-Objects.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 2.7 Type Predicates

The Emacs Lisp interpreter itself does not perform type checking on the actual arguments passed to functions when they are called. It could not do so, since function arguments in Lisp do not have declared data types, as they do in other programming languages. It is therefore up to the individual function to test whether each actual argument belongs to a type that the function can use.

All built-in functions do check the types of their actual arguments when appropriate, and signal a `wrong-type-argument` error if an argument is of the wrong type. For example, here is what happens if you pass an argument to `+` that it cannot handle:

    (+ 2 'a)
         error→ Wrong type argument: number-or-marker-p, a

If you want your program to handle different types differently, you must do explicit type checking. The most common way to check the type of an object is to call a *type predicate* function. Emacs has a type predicate for each type, as well as some predicates for combinations of types.

A type predicate function takes one argument; it returns `t` if the argument belongs to the appropriate type, and `nil` otherwise. Following a general Lisp convention for predicate functions, most type predicates’ names end with ‘`p`’.

Here is an example which uses the predicates `listp` to check for a list and `symbolp` to check for a symbol.

    (defun add-on (x)
      (cond ((symbolp x)
             ;; If X is a symbol, put it on LIST.
             (setq list (cons x list)))
            ((listp x)
             ;; If X is a list, add its elements to LIST.
             (setq list (append x list)))
            (t
             ;; We handle only symbols and lists.
             (error "Invalid argument %s in add-on" x))))

Here is a table of predefined type predicates, in alphabetical order, with references to further information.

*   `atom`

    See [atom](List_002drelated-Predicates.html).

*   `arrayp`

    See [arrayp](Array-Functions.html).

*   `bignump`

    See [floatp](Predicates-on-Numbers.html).

*   `bool-vector-p`

    See [bool-vector-p](Bool_002dVectors.html).

*   `booleanp`

    See [booleanp](nil-and-t.html).

*   `bufferp`

    See [bufferp](Buffer-Basics.html).

*   `byte-code-function-p`

    See [byte-code-function-p](Byte_002dCode-Type.html).

*   `case-table-p`

    See [case-table-p](Case-Tables.html).

*   `char-or-string-p`

    See [char-or-string-p](Predicates-for-Strings.html).

*   `char-table-p`

    See [char-table-p](Char_002dTables.html).

*   `commandp`

    See [commandp](Interactive-Call.html).

*   `condition-variable-p`

    See [condition-variable-p](Condition-Variables.html).

*   `consp`

    See [consp](List_002drelated-Predicates.html).

*   `custom-variable-p`

    See [custom-variable-p](Variable-Definitions.html).

*   `fixnump`

    See [floatp](Predicates-on-Numbers.html).

*   `floatp`

    See [floatp](Predicates-on-Numbers.html).

*   `fontp`

    See [Low-Level Font](Low_002dLevel-Font.html).

*   `frame-configuration-p`

    See [frame-configuration-p](Frame-Configurations.html).

*   `frame-live-p`

    See [frame-live-p](Deleting-Frames.html).

*   `framep`

    See [framep](Frames.html).

*   `functionp`

    See [functionp](Functions.html).

*   `hash-table-p`

    See [hash-table-p](Other-Hash.html).

*   `integer-or-marker-p`

    See [integer-or-marker-p](Predicates-on-Markers.html).

*   `integerp`

    See [integerp](Predicates-on-Numbers.html).

*   `keymapp`

    See [keymapp](Creating-Keymaps.html).

*   `keywordp`

    See [Constant Variables](Constant-Variables.html).

*   `listp`

    See [listp](List_002drelated-Predicates.html).

*   `markerp`

    See [markerp](Predicates-on-Markers.html).

*   `mutexp`

    See [mutexp](Mutexes.html).

*   `nlistp`

    See [nlistp](List_002drelated-Predicates.html).

*   `number-or-marker-p`

    See [number-or-marker-p](Predicates-on-Markers.html).

*   `numberp`

    See [numberp](Predicates-on-Numbers.html).

*   `overlayp`

    See [overlayp](Overlays.html).

*   `processp`

    See [processp](Processes.html).

*   `recordp`

    See [recordp](Record-Type.html).

*   `sequencep`

    See [sequencep](Sequence-Functions.html).

*   `string-or-null-p`

    See [string-or-null-p](Predicates-for-Strings.html).

*   `stringp`

    See [stringp](Predicates-for-Strings.html).

*   `subrp`

    See [subrp](Function-Cells.html).

*   `symbolp`

    See [symbolp](Symbols.html).

*   `syntax-table-p`

    See [syntax-table-p](Syntax-Tables.html).

*   `threadp`

    See [threadp](Basic-Thread-Functions.html).

*   `vectorp`

    See [vectorp](Vectors.html).

*   `wholenump`

    See [wholenump](Predicates-on-Numbers.html).

*   `window-configuration-p`

    See [window-configuration-p](Window-Configurations.html).

*   `window-live-p`

    See [window-live-p](Deleting-Windows.html).

*   `windowp`

    See [windowp](Basic-Windows.html).

The most general way to check the type of an object is to call the function `type-of`. Recall that each object belongs to one and only one primitive type; `type-of` tells you which one (see [Lisp Data Types](Lisp-Data-Types.html)). But `type-of` knows nothing about non-primitive types. In most cases, it is more convenient to use type predicates than `type-of`.

*   Function: **type-of** *object*

    This function returns a symbol naming the primitive type of `object`. The value is one of the symbols `bool-vector`, `buffer`, `char-table`, `compiled-function`, `condition-variable`, `cons`, `finalizer`, `float`, `font-entity`, `font-object`, `font-spec`, `frame`, `hash-table`, `integer`, `marker`, `mutex`, `overlay`, `process`, `string`, `subr`, `symbol`, `thread`, `vector`, `window`, or `window-configuration`. However, if `object` is a record, the type specified by its first slot is returned; [Records](Records.html).

        (type-of 1)
             ⇒ integer

    <!---->

        (type-of 'nil)
             ⇒ symbol
        (type-of '())    ; () is nil.
             ⇒ symbol
        (type-of '(x))
             ⇒ cons
        (type-of (record 'foo))
             ⇒ foo

Next: [Equality Predicates](Equality-Predicates.html), Previous: [Circular Objects](Circular-Objects.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
