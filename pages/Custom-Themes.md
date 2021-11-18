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

Previous: [Applying Customizations](Applying-Customizations.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 15.6 Custom Themes

*Custom themes* are collections of settings that can be enabled or disabled as a unit. See [Custom Themes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html#Custom-Themes) in The GNU Emacs Manual. Each Custom theme is defined by an Emacs Lisp source file, which should follow the conventions described in this section. (Instead of writing a Custom theme by hand, you can also create one using a Customize-like interface; see [Creating Custom Themes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html#Creating-Custom-Themes) in The GNU Emacs Manual.)

A Custom theme file should be named `foo-theme.el`, where `foo` is the theme name. The first Lisp form in the file should be a call to `deftheme`, and the last form should be a call to `provide-theme`.

*   Macro: **deftheme** *theme \&optional doc*

    This macro declares `theme` (a symbol) as the name of a Custom theme. The optional argument `doc` should be a string describing the theme; this is the description shown when the user invokes the `describe-theme` command or types `?` in the ‘`*Custom Themes*`’ buffer.

    Two special theme names are disallowed (using them causes an error): `user` is a dummy theme that stores the user’s direct customization settings, and `changed` is a dummy theme that stores changes made outside of the Customize system.

<!---->

*   Macro: **provide-theme** *theme*

    This macro declares that the theme named `theme` has been fully specified.

In between `deftheme` and `provide-theme` are Lisp forms specifying the theme settings: usually a call to `custom-theme-set-variables` and/or a call to `custom-theme-set-faces`.

*   Function: **custom-theme-set-variables** *theme \&rest args*

    This function specifies the Custom theme `theme`’s variable settings. `theme` should be a symbol. Each argument in `args` should be a list of the form

        (var expression [now [request [comment]]])

    where the list entries have the same meanings as in `custom-set-variables`. See [Applying Customizations](Applying-Customizations.html).

<!---->

*   Function: **custom-theme-set-faces** *theme \&rest args*

    This function specifies the Custom theme `theme`’s face settings. `theme` should be a symbol. Each argument in `args` should be a list of the form

        (face spec [now [comment]])

    where the list entries have the same meanings as in `custom-set-faces`. See [Applying Customizations](Applying-Customizations.html).

In theory, a theme file can also contain other Lisp forms, which would be evaluated when loading the theme, but that is bad form. To protect against loading themes containing malicious code, Emacs displays the source file and asks for confirmation from the user before loading any non-built-in theme for the first time. As such, themes are not ordinarily byte-compiled, and source files always take precedence when Emacs is looking for a theme to load.

The following functions are useful for programmatically enabling and disabling themes:

*   Function: **custom-theme-p** *theme*

    This function return a non-`nil` value if `theme` (a symbol) is the name of a Custom theme (i.e., a Custom theme which has been loaded into Emacs, whether or not the theme is enabled). Otherwise, it returns `nil`.

<!---->

*   Variable: **custom-known-themes**

    The value of this variable is a list of themes loaded into Emacs. Each theme is represented by a Lisp symbol (the theme name). The default value of this variable is a list containing two dummy themes: `(user changed)`. The `changed` theme stores settings made before any Custom themes are applied (e.g., variables set outside of Customize). The `user` theme stores settings the user has customized and saved. Any additional themes declared with the `deftheme` macro are added to the front of this list.

<!---->

*   Command: **load-theme** *theme \&optional no-confirm no-enable*

    This function loads the Custom theme named `theme` from its source file, looking for the source file in the directories specified by the variable `custom-theme-load-path`. See [Custom Themes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html#Custom-Themes) in The GNU Emacs Manual. It also *enables* the theme (unless the optional argument `no-enable` is non-`nil`), causing its variable and face settings to take effect. It prompts the user for confirmation before loading the theme, unless the optional argument `no-confirm` is non-`nil`.

<!---->

*   Command: **enable-theme** *theme*

    This function enables the Custom theme named `theme`. It signals an error if no such theme has been loaded.

<!---->

*   Command: **disable-theme** *theme*

    This function disables the Custom theme named `theme`. The theme remains loaded, so that a subsequent call to `enable-theme` will re-enable it.

Previous: [Applying Customizations](Applying-Customizations.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
