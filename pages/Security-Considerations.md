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

Previous: [Dynamic Libraries](Dynamic-Libraries.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 40.22 Security Considerations

Like any application, Emacs can be run in a secure environment, where the operating system enforces rules about access and the like. With some care, Emacs-based applications can also be part of a security perimeter that checks such rules. Although the default settings for Emacs work well for a typical software development environment, they may require adjustment in environments containing untrusted users that may include attackers. Here is a compendium of security issues that may be helpful if you are developing such applications. It is by no means complete; it is intended to give you an idea of the security issues involved, rather than to be a security checklist.

*   File local variables

    A file that Emacs visits can contain variable settings that affect the buffer visiting that file; See [File Local Variables](File-Local-Variables.html). Similarly, a directory can specify local variable values common to all files in that directory; see [Directory Local Variables](Directory-Local-Variables.html). Although Emacs takes some effort to protect against misuse of these variables, a security hole can be created merely by a package setting `safe-local-variable` too optimistically, a problem that is all too common. To disable this feature for both files and directories, set `enable-local-variables` to `nil`.

*   Access control

    Although Emacs normally respects access permissions of the underlying operating system, in some cases it handles accesses specially. For example, file names can have handlers that treat the files specially, with their own access checking. See [Magic File Names](Magic-File-Names.html). Also, a buffer can be read-only even if the corresponding file is writable, and vice versa, which can result in messages such as ‘`File passwd is write-protected; try to save anyway? (yes or no)`’. See [Read Only Buffers](Read-Only-Buffers.html).

*   Authentication

    Emacs has several functions that deal with passwords, e.g., `read-passwd`. See [Reading a Password](Reading-a-Password.html). Although these functions do not attempt to broadcast passwords to the world, their implementations are not proof against determined attackers with access to Emacs internals. For example, even if Elisp code uses `clear-string` to scrub a password from its memory after using it, remnants of the password may still reside in the garbage-collected free list. See [Modifying Strings](Modifying-Strings.html).

*   Code injection

    Emacs can send commands to many other applications, and applications should take care that strings sent as operands of these commands are not misinterpreted as directives. For example, when using a shell command to rename a file `a` to `b`, do not simply use the string `mv a b`, because either file name might start with ‘`-`’, or might contain shell metacharacters like ‘`;`’. Although functions like `shell-quote-argument` can help avoid this sort of problem, they are not panaceas; for example, on a POSIX platform `shell-quote-argument` quotes shell metacharacters but not leading ‘`-`’. On MS-Windows, quoting for ‘`%`’ assumes none of the environment variables have ‘`^`’ in their name. See [Shell Arguments](Shell-Arguments.html). Typically it is safer to use `call-process` than a subshell. See [Synchronous Processes](Synchronous-Processes.html). And it is safer yet to use builtin Emacs functions; for example, use `(rename-file "a" "b" t)` instead of invoking `mv`. See [Changing Files](Changing-Files.html).

*   Coding systems

    Emacs attempts to infer the coding systems of the files and network connections it accesses. See [Coding Systems](Coding-Systems.html). If Emacs infers incorrectly, or if the other parties to the network connection disagree with Emacs’s inferences, the resulting system could be unreliable. Also, even when it infers correctly, Emacs often can use bytes that other programs cannot. For example, although to Emacs the null byte is just a character like any other, many other applications treat it as a string terminator and mishandle strings or files containing null bytes.

*   Environment and configuration variables

    POSIX specifies several environment variables that can affect how Emacs behaves. Any environment variable whose name consists entirely of uppercase ASCII letters, digits, and the underscore may affect the internal behavior of Emacs. Emacs uses several such variables, e.g., `EMACSLOADPATH`. See [Library Search](Library-Search.html). On some platforms some environment variables (e.g., `PATH`, `POSIXLY_CORRECT`, `SHELL`, `TMPDIR`) need to have properly-configured values in order to get standard behavior for any utility Emacs might invoke. Even seemingly-benign variables like `TZ` may have security implications. See [System Environment](System-Environment.html).

    Emacs has customization and other variables with similar considerations. For example, if the variable `shell-file-name` specifies a shell with nonstandard behavior, an Emacs-based application may misbehave.

*   Installation

    When Emacs is installed, if the installation directory hierarchy can be modified by untrusted users, the application cannot be trusted. This applies also to the directory hierarchies of the programs that Emacs uses, and of the files that Emacs reads and writes.

*   Network access

    Emacs often accesses the network, and you may want to configure it to avoid network accesses that it would normally do. For example, unless you set `tramp-mode` to `nil`, file names using a certain syntax are interpreted as being network files, and are retrieved across the network. See [The Tramp Manual](https://www.gnu.org/software/emacs/manual/html_node/tramp/index.html#Top) in The Tramp Manual.

*   Race conditions

    Emacs applications have the same sort of race-condition issues that other applications do. For example, even when `(file-readable-p "foo.txt")` returns `t`, it could be that `foo.txt` is unreadable because some other program changed the file’s permissions between the call to `file-readable-p` and now. See [Testing Accessibility](Testing-Accessibility.html).

*   Resource limits

    When Emacs exhausts memory or other operating system resources, its behavior can be less reliable, in that computations that ordinarily run to completion may abort back to the top level. This may cause Emacs to neglect operations that it normally would have done.

Previous: [Dynamic Libraries](Dynamic-Libraries.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
