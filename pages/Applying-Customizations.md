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

Next: [Custom Themes](Custom-Themes.html), Previous: [Customization Types](Customization-Types.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 15.5 Applying Customizations

The following functions are responsible for installing the user’s customization settings for variables and faces, respectively. When the user invokes ‘`Save for future sessions`’ in the Customize interface, that takes effect by writing a `custom-set-variables` and/or a `custom-set-faces` form into the custom file, to be evaluated the next time Emacs starts.

*   Function: **custom-set-variables** *\&rest args*

    This function installs the variable customizations specified by `args`. Each argument in `args` should have the form

        (var expression [now [request [comment]]])

    `var` is a variable name (a symbol), and `expression` is an expression which evaluates to the desired customized value.

    If the `defcustom` form for `var` has been evaluated prior to this `custom-set-variables` call, `expression` is immediately evaluated, and the variable’s value is set to the result. Otherwise, `expression` is stored into the variable’s `saved-value` property, to be evaluated when the relevant `defcustom` is called (usually when the library defining that variable is loaded into Emacs).

    The `now`, `request`, and `comment` entries are for internal use only, and may be omitted. `now`, if non-`nil`, means to set the variable’s value now, even if the variable’s `defcustom` form has not been evaluated. `request` is a list of features to be loaded immediately (see [Named Features](Named-Features.html)). `comment` is a string describing the customization.

<!---->

*   Function: **custom-set-faces** *\&rest args*

    This function installs the face customizations specified by `args`. Each argument in `args` should have the form

        (face spec [now [comment]])

    `face` is a face name (a symbol), and `spec` is the customized face specification for that face (see [Defining Faces](Defining-Faces.html)).

    The `now` and `comment` entries are for internal use only, and may be omitted. `now`, if non-`nil`, means to install the face specification now, even if the `defface` form has not been evaluated. `comment` is a string describing the customization.

Next: [Custom Themes](Custom-Themes.html), Previous: [Customization Types](Customization-Types.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
