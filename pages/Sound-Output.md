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

Next: [X11 Keysyms](X11-Keysyms.html), Previous: [Terminal Output](Terminal-Output.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 40.15 Sound Output

To play sound using Emacs, use the function `play-sound`. Only certain systems are supported; if you call `play-sound` on a system which cannot really do the job, it gives an error.

The sound must be stored as a file in RIFF-WAVE format (‘`.wav`’) or Sun Audio format (‘`.au`’).

*   Function: **play-sound** *sound*

    This function plays a specified sound. The argument, `sound`, has the form `(sound properties...)`, where the `properties` consist of alternating keywords (particular symbols recognized specially) and values corresponding to them.

    Here is a table of the keywords that are currently meaningful in `sound`, and their meanings:

    *   `:file file`

        This specifies the file containing the sound to play. If the file name is not absolute, it is expanded against the directory `data-directory`.

    *   `:data data`

        This specifies the sound to play without need to refer to a file. The value, `data`, should be a string containing the same bytes as a sound file. We recommend using a unibyte string.

    *   `:volume volume`

        This specifies how loud to play the sound. It should be a number in the range of 0 to 1. The default is to use whatever volume has been specified before.

    *   `:device device`

        This specifies the system device on which to play the sound, as a string. The default device is system-dependent.

    Before actually playing the sound, `play-sound` calls the functions in the list `play-sound-functions`. Each function is called with one argument, `sound`.

<!---->

*   Command: **play-sound-file** *file \&optional volume device*

    This function is an alternative interface to playing a sound `file` specifying an optional `volume` and `device`.

<!---->

*   Variable: **play-sound-functions**

    A list of functions to be called before playing a sound. Each function is called with one argument, a property list that describes the sound.

Next: [X11 Keysyms](X11-Keysyms.html), Previous: [Terminal Output](Terminal-Output.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
