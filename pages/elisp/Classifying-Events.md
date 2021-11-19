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

Next: [Accessing Mouse](Accessing-Mouse.html), Previous: [Event Examples](Event-Examples.html), Up: [Input Events](Input-Events.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.7.12 Classifying Events

Every event has an *event type*, which classifies the event for key binding purposes. For a keyboard event, the event type equals the event value; thus, the event type for a character is the character, and the event type for a function key symbol is the symbol itself. For events that are lists, the event type is the symbol in the CAR of the list. Thus, the event type is always a symbol or a character.

Two events of the same type are equivalent where key bindings are concerned; thus, they always run the same command. That does not necessarily mean they do the same things, however, as some commands look at the whole event to decide what to do. For example, some commands use the location of a mouse event to decide where in the buffer to act.

Sometimes broader classifications of events are useful. For example, you might want to ask whether an event involved the `META` key, regardless of which other key or mouse button was used.

The functions `event-modifiers` and `event-basic-type` are provided to get such information conveniently.

*   Function: **event-modifiers** *event*

    This function returns a list of the modifiers that `event` has. The modifiers are symbols; they include `shift`, `control`, `meta`, `alt`, `hyper` and `super`. In addition, the modifiers list of a mouse event symbol always contains one of `click`, `drag`, and `down`. For double or triple events, it also contains `double` or `triple`.

    The argument `event` may be an entire event object, or just an event type. If `event` is a symbol that has never been used in an event that has been read as input in the current Emacs session, then `event-modifiers` can return `nil`, even when `event` actually has modifiers.

    Here are some examples:

        (event-modifiers ?a)
             ⇒ nil
        (event-modifiers ?A)
             ⇒ (shift)
        (event-modifiers ?\C-a)
             ⇒ (control)
        (event-modifiers ?\C-%)
             ⇒ (control)
        (event-modifiers ?\C-\S-a)
             ⇒ (control shift)
        (event-modifiers 'f5)
             ⇒ nil
        (event-modifiers 's-f5)
             ⇒ (super)
        (event-modifiers 'M-S-f5)
             ⇒ (meta shift)
        (event-modifiers 'mouse-1)
             ⇒ (click)
        (event-modifiers 'down-mouse-1)
             ⇒ (down)

    The modifiers list for a click event explicitly contains `click`, but the event symbol name itself does not contain ‘`click`’. Similarly, the modifiers list for an ASCII control character, such as ‘`C-a`’, contains `control`, even though reading such an event via `read-char` will return the value 1 with the control modifier bit removed.

<!---->

*   Function: **event-basic-type** *event*

    This function returns the key or mouse button that `event` describes, with all modifiers removed. The `event` argument is as in `event-modifiers`. For example:

        (event-basic-type ?a)
             ⇒ 97
        (event-basic-type ?A)
             ⇒ 97
        (event-basic-type ?\C-a)
             ⇒ 97
        (event-basic-type ?\C-\S-a)
             ⇒ 97
        (event-basic-type 'f5)
             ⇒ f5
        (event-basic-type 's-f5)
             ⇒ f5
        (event-basic-type 'M-S-f5)
             ⇒ f5
        (event-basic-type 'down-mouse-1)
             ⇒ mouse-1

<!---->

*   Function: **mouse-movement-p** *object*

    This function returns non-`nil` if `object` is a mouse movement event. See [Motion Events](Motion-Events.html).

<!---->

*   Function: **event-convert-list** *list*

    This function converts a list of modifier names and a basic event type to an event type which specifies all of them. The basic event type must be the last element of the list. For example,

        (event-convert-list '(control ?a))
             ⇒ 1
        (event-convert-list '(control meta ?a))
             ⇒ -134217727
        (event-convert-list '(control super f1))
             ⇒ C-s-f1

Next: [Accessing Mouse](Accessing-Mouse.html), Previous: [Event Examples](Event-Examples.html), Up: [Input Events](Input-Events.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
