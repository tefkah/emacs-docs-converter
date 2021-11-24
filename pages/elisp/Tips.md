

## Appendix D Tips and Conventions

This chapter describes no additional features of Emacs Lisp. Instead it gives advice on making effective use of the features described in the previous chapters, and describes conventions Emacs Lisp programmers should follow.

You can automatically check some of the conventions described below by running the command `M-x checkdoc RET` when visiting a Lisp file. It cannot check all of the conventions, and not all the warnings it gives necessarily correspond to problems, but it is worth examining them all. Alternatively, use the command `M-x checkdoc-current-buffer RET` to check the conventions in the current buffer, or `checkdoc-file` when you want to check a file in batch mode, e.g., with a command run by `M-x compile RET`.

|                                                           |    |                                               |
| :-------------------------------------------------------- | -- | :-------------------------------------------- |
| • [Coding Conventions](Coding-Conventions.html)           |    | Conventions for clean and robust programs.    |
| • [Key Binding Conventions](Key-Binding-Conventions.html) |    | Which keys should be bound by which programs. |
| • [Programming Tips](Programming-Tips.html)               |    | Making Emacs code fit smoothly in Emacs.      |
| • [Compilation Tips](Compilation-Tips.html)               |    | Making compiled code run fast.                |
| • [Warning Tips](Warning-Tips.html)                       |    | Turning off compiler warnings.                |
| • [Documentation Tips](Documentation-Tips.html)           |    | Writing readable documentation strings.       |
| • [Comment Tips](Comment-Tips.html)                       |    | Conventions for writing comments.             |
| • [Library Headers](Library-Headers.html)                 |    | Standard headers for library packages.        |
