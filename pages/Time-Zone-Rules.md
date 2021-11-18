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

Next: [Time Conversion](Time-Conversion.html), Previous: [Time of Day](Time-of-Day.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 40.6 Time Zone Rules

The default time zone is determined by the `TZ` environment variable. See [System Environment](System-Environment.html). For example, you can tell Emacs to default to Universal Time with `(setenv "TZ" "UTC0")`. If `TZ` is not in the environment, Emacs uses system wall clock time, which is a platform-dependent default time zone.

The set of supported `TZ` strings is system-dependent. GNU and many other systems support the tzdata database, e.g., ‘`"America/New_York"`’ specifies the time zone and daylight saving time history for locations near New York City. GNU and most other systems support POSIX-style `TZ` strings, e.g., ‘`"EST+5EDT,M4.1.0/2,M10.5.0/2"`’ specifies the rules used in New York from 1987 through 2006. All systems support the string ‘`"UTC0"`’ meaning Universal Time.

Functions that convert to and from local time accept an optional *time zone rule* argument, which specifies the conversion’s time zone and daylight saving time history. If the time zone rule is omitted or `nil`, the conversion uses Emacs’s default time zone. If it is `t`, the conversion uses Universal Time. If it is `wall`, the conversion uses the system wall clock time. If it is a string, the conversion uses the time zone rule equivalent to setting `TZ` to that string. If it is a list (`offset` `abbr`), where `offset` is an integer number of seconds east of Universal Time and `abbr` is a string, the conversion uses a fixed time zone with the given offset and abbreviation. An integer `offset` is treated as if it were (`offset` `abbr`), where `abbr` is a numeric abbreviation on POSIX-compatible platforms and is unspecified on MS-Windows.

*   Function: **current-time-zone** *\&optional time zone*

    This function returns a list describing the time zone that the user is in.

    The value has the form `(offset abbr)`. Here `offset` is an integer giving the number of seconds ahead of Universal Time (east of Greenwich). A negative value means west of Greenwich. The second element, `abbr`, is a string giving an abbreviation for the time zone, e.g., ‘`"CST"`’ for China Standard Time or for U.S. Central Standard Time. Both elements can change when daylight saving time begins or ends; if the user has specified a time zone that does not use a seasonal time adjustment, then the value is constant through time.

    If the operating system doesn’t supply all the information necessary to compute the value, the unknown elements of the list are `nil`.

    The argument `time`, if given, specifies a time value to analyze instead of the current time. The optional argument `zone` defaults to the current time zone rule. The operating system limits the range of time and zone values.

Next: [Time Conversion](Time-Conversion.html), Previous: [Time of Day](Time-of-Day.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
