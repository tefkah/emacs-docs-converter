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

Next: [Lazy Properties](Lazy-Properties.html), Previous: [Format Properties](Format-Properties.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.19.6 Stickiness of Text Properties

Self-inserting characters, the ones that get inserted into a buffer when the user types them (see [Commands for Insertion](Commands-for-Insertion.html)), normally take on the same properties as the preceding character. This is called *inheritance* of properties.

By contrast, a Lisp program can do insertion with inheritance or without, depending on the choice of insertion primitive. The ordinary text insertion functions, such as `insert`, do not inherit any properties. They insert text with precisely the properties of the string being inserted, and no others. This is correct for programs that copy text from one context to another—for example, into or out of the kill ring. To insert with inheritance, use the special primitives described in this section. Self-inserting characters inherit properties because they work using these primitives.

When you do insertion with inheritance, *which* properties are inherited, and from where, depends on which properties are *sticky*. Insertion after a character inherits those of its properties that are *rear-sticky*. Insertion before a character inherits those of its properties that are *front-sticky*. When both sides offer different sticky values for the same property, the previous character’s value takes precedence.

By default, a text property is rear-sticky but not front-sticky; thus, the default is to inherit all the properties of the preceding character, and nothing from the following character.

You can control the stickiness of various text properties with two specific text properties, `front-sticky` and `rear-nonsticky`, and with the variable `text-property-default-nonsticky`. You can use the variable to specify a different default for a given property. You can use those two text properties to make any specific properties sticky or nonsticky in any particular part of the text.

If a character’s `front-sticky` property is `t`, then all its properties are front-sticky. If the `front-sticky` property is a list, then the sticky properties of the character are those whose names are in the list. For example, if a character has a `front-sticky` property whose value is `(face read-only)`, then insertion before the character can inherit its `face` property and its `read-only` property, but no others.

The `rear-nonsticky` property works the opposite way. Most properties are rear-sticky by default, so the `rear-nonsticky` property says which properties are *not* rear-sticky. If a character’s `rear-nonsticky` property is `t`, then none of its properties are rear-sticky. If the `rear-nonsticky` property is a list, properties are rear-sticky *unless* their names are in the list.

*   Variable: **text-property-default-nonsticky**

    This variable holds an alist which defines the default rear-stickiness of various text properties. Each element has the form `(property . nonstickiness)`, and it defines the stickiness of a particular text property, `property`.

    If `nonstickiness` is non-`nil`, this means that the property `property` is rear-nonsticky by default. Since all properties are front-nonsticky by default, this makes `property` nonsticky in both directions by default.

    The text properties `front-sticky` and `rear-nonsticky`, when used, take precedence over the default `nonstickiness` specified in `text-property-default-nonsticky`.

Here are the functions that insert text with inheritance of properties:

*   Function: **insert-and-inherit** *\&rest strings*

    Insert the strings `strings`, just like the function `insert`, but inherit any sticky properties from the adjoining text.

<!---->

*   Function: **insert-before-markers-and-inherit** *\&rest strings*

    Insert the strings `strings`, just like the function `insert-before-markers`, but inherit any sticky properties from the adjoining text.

See [Insertion](Insertion.html), for the ordinary insertion functions which do not inherit.

Next: [Lazy Properties](Lazy-Properties.html), Previous: [Format Properties](Format-Properties.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
