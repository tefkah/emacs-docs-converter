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

Next: [Standard Properties](Standard-Properties.html), Up: [Symbol Properties](Symbol-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 9.4.1 Accessing Symbol Properties

The following functions can be used to access symbol properties.

*   Function: **get** *symbol property*

    This function returns the value of the property named `property` in `symbol`’s property list. If there is no such property, it returns `nil`. Thus, there is no distinction between a value of `nil` and the absence of the property.

    The name `property` is compared with the existing property names using `eq`, so any object is a legitimate property.

    See `put` for an example.

<!---->

*   Function: **put** *symbol property value*

    This function puts `value` onto `symbol`’s property list under the property name `property`, replacing any previous property value. The `put` function returns `value`.

        (put 'fly 'verb 'transitive)
             ⇒'transitive
        (put 'fly 'noun '(a buzzing little bug))
             ⇒ (a buzzing little bug)
        (get 'fly 'verb)
             ⇒ transitive
        (symbol-plist 'fly)
             ⇒ (verb transitive noun (a buzzing little bug))

<!---->

*   Function: **symbol-plist** *symbol*

    This function returns the property list of `symbol`.

<!---->

*   Function: **setplist** *symbol plist*

    This function sets `symbol`’s property list to `plist`. Normally, `plist` should be a well-formed property list, but this is not enforced. The return value is `plist`.

        (setplist 'foo '(a 1 b (2 3) c nil))
             ⇒ (a 1 b (2 3) c nil)
        (symbol-plist 'foo)
             ⇒ (a 1 b (2 3) c nil)

    For symbols in special obarrays, which are not used for ordinary purposes, it may make sense to use the property list cell in a nonstandard fashion; in fact, the abbrev mechanism does so (see [Abbrevs](Abbrevs.html)).

    You could define `put` in terms of `setplist` and `plist-put`, as follows:

        (defun put (symbol prop value)
          (setplist symbol
                    (plist-put (symbol-plist symbol) prop value)))

<!---->

*   Function: **function-get** *symbol property \&optional autoload*

    This function is identical to `get`, except that if `symbol` is the name of a function alias, it looks in the property list of the symbol naming the actual function. See [Defining Functions](Defining-Functions.html). If the optional argument `autoload` is non-`nil`, and `symbol` is auto-loaded, this function will try to autoload it, since autoloading might set `property` of `symbol`. If `autoload` is the symbol `macro`, only try autoloading if `symbol` is an auto-loaded macro.

<!---->

*   Function: **function-put** *function property value*

    This function sets `property` of `function` to `value`. `function` should be a symbol. This function is preferred to calling `put` for setting properties of a function, because it will allow us some day to implement remapping of old properties to new ones.

Next: [Standard Properties](Standard-Properties.html), Up: [Symbol Properties](Symbol-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
