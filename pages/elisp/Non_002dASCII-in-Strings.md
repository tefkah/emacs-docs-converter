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

Next: [Nonprinting Characters](Nonprinting-Characters.html), Previous: [Syntax for Strings](Syntax-for-Strings.html), Up: [String Type](String-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.8.2 Non-ASCII Characters in Strings

There are two text representations for non-ASCII characters in Emacs strings: multibyte and unibyte (see [Text Representations](Text-Representations.html)). Roughly speaking, unibyte strings store raw bytes, while multibyte strings store human-readable text. Each character in a unibyte string is a byte, i.e., its value is between 0 and 255. By contrast, each character in a multibyte string may have a value between 0 to 4194303 (see [Character Type](Character-Type.html)). In both cases, characters above 127 are non-ASCII.

You can include a non-ASCII character in a string constant by writing it literally. If the string constant is read from a multibyte source, such as a multibyte buffer or string, or a file that would be visited as multibyte, then Emacs reads each non-ASCII character as a multibyte character and automatically makes the string a multibyte string. If the string constant is read from a unibyte source, then Emacs reads the non-ASCII character as unibyte, and makes the string unibyte.

Instead of writing a character literally into a multibyte string, you can write it as its character code using an escape sequence. See [General Escape Syntax](General-Escape-Syntax.html), for details about escape sequences.

If you use any Unicode-style escape sequence ‘`\uNNNN`’ or ‘`\U00NNNNNN`’ in a string constant (even for an ASCII character), Emacs automatically assumes that it is multibyte.

You can also use hexadecimal escape sequences (‘`\xn`’) and octal escape sequences (‘`\n`’) in string constants. **But beware:** If a string constant contains hexadecimal or octal escape sequences, and these escape sequences all specify unibyte characters (i.e., less than 256), and there are no other literal non-ASCII characters or Unicode-style escape sequences in the string, then Emacs automatically assumes that it is a unibyte string. That is to say, it assumes that all non-ASCII characters occurring in the string are 8-bit raw bytes.

In hexadecimal and octal escape sequences, the escaped character code may contain a variable number of digits, so the first subsequent character which is not a valid hexadecimal or octal digit terminates the escape sequence. If the next character in a string could be interpreted as a hexadecimal or octal digit, write ‘`\ `’<!-- /@w --> (backslash and space) to terminate the escape sequence. For example, ‘`\xe0\ `’<!-- /@w --> represents one character, ‘`a`’ with grave accent. ‘`\ `’<!-- /@w --> in a string constant is just like backslash-newline; it does not contribute any character to the string, but it does terminate any preceding hex escape.

Next: [Nonprinting Characters](Nonprinting-Characters.html), Previous: [Syntax for Strings](Syntax-for-Strings.html), Up: [String Type](String-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
