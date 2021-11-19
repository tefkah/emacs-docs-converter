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

Next: [Variable Aliases](Variable-Aliases.html), Previous: [Directory Local Variables](Directory-Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.14 Connection Local Variables

Connection-local variables provide a general mechanism for different variable settings in buffers with a remote connection. They are bound and set depending on the remote connection a buffer is dedicated to.

*   Function: **connection-local-set-profile-variables** *profile variables*

    This function defines a set of variable settings for the connection `profile`, which is a symbol. You can later assign the connection profile to one or more remote connections, and Emacs will apply those variable settings to all process buffers for those connections. The list in `variables` is an alist of the form `(name . value)`. Example:

        (connection-local-set-profile-variables
          'remote-bash
          '((shell-file-name . "/bin/bash")
            (shell-command-switch . "-c")
            (shell-interactive-switch . "-i")
            (shell-login-switch . "-l")))

    ```
    ```

        (connection-local-set-profile-variables
          'remote-ksh
          '((shell-file-name . "/bin/ksh")
            (shell-command-switch . "-c")
            (shell-interactive-switch . "-i")
            (shell-login-switch . "-l")))

    ```
    ```

        (connection-local-set-profile-variables
          'remote-null-device
          '((null-device . "/dev/null")))

<!---->

*   Variable: **connection-local-profile-alist**

    This alist holds the connection profile symbols and the associated variable settings. It is updated by `connection-local-set-profile-variables`.

<!---->

*   Function: **connection-local-set-profiles** *criteria \&rest profiles*

    This function assigns `profiles`, which are symbols, to all remote connections identified by `criteria`. `criteria` is a plist identifying a connection and the application using this connection. Property names might be `:application`, `:protocol`, `:user` and `:machine`. The property value of `:application` is a symbol, all other property values are strings. All properties are optional; if `criteria` is `nil`, it always applies. Example:

        (connection-local-set-profiles
          '(:application 'tramp :protocol "ssh" :machine "localhost")
          'remote-bash 'remote-null-device)

    ```
    ```

        (connection-local-set-profiles
          '(:application 'tramp :protocol "sudo"
            :user "root" :machine "localhost")
          'remote-ksh 'remote-null-device)

    If `criteria` is `nil`, it applies for all remote connections. Therefore, the example above would be equivalent to

        (connection-local-set-profiles
          '(:application 'tramp :protocol "ssh" :machine "localhost")
          'remote-bash)

    ```
    ```

        (connection-local-set-profiles
          '(:application 'tramp :protocol "sudo"
            :user "root" :machine "localhost")
          'remote-ksh)

    ```
    ```

        (connection-local-set-profiles
          nil 'remote-null-device)

    Any connection profile of `profiles` must have been already defined by `connection-local-set-profile-variables`.

<!---->

*   Variable: **connection-local-criteria-alist**

    This alist contains connection criteria and their assigned profile names. The function `connection-local-set-profiles` updates this list.

<!---->

*   Function: **hack-connection-local-variables** *criteria*

    This function collects applicable connection-local variables associated with `criteria` in `connection-local-variables-alist`, without applying them. Example:

        (hack-connection-local-variables
          '(:application 'tramp :protocol "ssh" :machine "localhost"))

    ```
    ```

        connection-local-variables-alist
             ⇒ ((null-device . "/dev/null")
                (shell-login-switch . "-l")
                (shell-interactive-switch . "-i")
                (shell-command-switch . "-c")
                (shell-file-name . "/bin/bash"))

<!---->

*   Function: **hack-connection-local-variables-apply** *criteria*

    This function looks for connection-local variables according to `criteria`, and immediately applies them in the current buffer.

<!---->

*   Macro: **with-connection-local-variables** *\&rest body*

    All connection-local variables, which are specified by `default-directory`, are applied.

    After that, `body` is executed, and the connection-local variables are unwound. Example:

        (connection-local-set-profile-variables
          'remote-perl
          '((perl-command-name . "/usr/local/bin/perl")
            (perl-command-switch . "-e %s")))

    ```
    ```

        (connection-local-set-profiles
          '(:application 'tramp :protocol "ssh" :machine "remotehost")
          'remote-perl)

    ```
    ```

        (let ((default-directory "/ssh:remotehost:/working/dir/"))
          (with-connection-local-variables
            do something useful))

<!---->

*   Variable: **enable-connection-local-variables**

    If `nil`, connection-local variables are ignored. This variable shall be changed temporarily only in special modes.

Next: [Variable Aliases](Variable-Aliases.html), Previous: [Directory Local Variables](Directory-Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
