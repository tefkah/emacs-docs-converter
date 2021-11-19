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

Next: [Motion](Motion.html), Up: [Positions](Positions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 30.1 Point

*Point* is a special buffer position used by many editing commands, including the self-inserting typed characters and text insertion functions. Other commands move point through the text to allow editing and insertion at different places.

Like other positions, point designates a place between two characters (or before the first character, or after the last character), rather than a particular character. Usually terminals display the cursor over the character that immediately follows point; point is actually before the character on which the cursor sits.

The value of point is a number no less than 1, and no greater than the buffer size plus 1. If narrowing is in effect (see [Narrowing](Narrowing.html)), then point is constrained to fall within the accessible portion of the buffer (possibly at one end of it).

Each buffer has its own value of point, which is independent of the value of point in other buffers. Each window also has a value of point, which is independent of the value of point in other windows on the same buffer. This is why point can have different values in various windows that display the same buffer. When a buffer appears in only one window, the buffer’s point and the window’s point normally have the same value, so the distinction is rarely important. See [Window Point](Window-Point.html), for more details.

*   Function: **point**

    This function returns the value of point in the current buffer, as an integer.

        (point)
             ⇒ 175

<!---->

*   Function: **point-min**

    This function returns the minimum accessible value of point in the current buffer. This is normally 1, but if narrowing is in effect, it is the position of the start of the region that you narrowed to. (See [Narrowing](Narrowing.html).)

<!---->

*   Function: **point-max**

    This function returns the maximum accessible value of point in the current buffer. This is `(1+ (buffer-size))`, unless narrowing is in effect, in which case it is the position of the end of the region that you narrowed to. (See [Narrowing](Narrowing.html).)

<!---->

*   Function: **buffer-end** *flag*

    This function returns `(point-max)` if `flag` is greater than 0, `(point-min)` otherwise. The argument `flag` must be a number.

<!---->

*   Function: **buffer-size** *\&optional buffer*

    This function returns the total number of characters in the current buffer. In the absence of any narrowing (see [Narrowing](Narrowing.html)), `point-max` returns a value one larger than this.

    If you specify a buffer, `buffer`, then the value is the size of `buffer`.

        (buffer-size)
             ⇒ 35

    <!---->

        (point-max)
             ⇒ 36

Next: [Motion](Motion.html), Up: [Positions](Positions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
