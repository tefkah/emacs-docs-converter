

Next: [Documentation Tips](Documentation-Tips.html), Previous: [Compilation Tips](Compilation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### D.5 Tips for Avoiding Compiler Warnings

*   Try to avoid compiler warnings about undefined free variables, by adding dummy `defvar` definitions for these variables, like this:

    ```lisp
    (defvar foo)
    ```

    Such a definition has no effect except to tell the compiler not to warn about uses of the variable `foo` in this file.

*   Similarly, to avoid a compiler warning about an undefined function that you know *will* be defined, use a `declare-function` statement (see [Declaring Functions](Declaring-Functions.html)).

*   If you use many functions, macros, and variables from a certain file, you can add a `require` (see [require](Named-Features.html)) for that package to avoid compilation warnings for them, like this:

    ```lisp
    (require 'foo)
    ```

    If you need only macros from some file, you can require it only at compile time (see [Eval During Compile](Eval-During-Compile.html)). For instance,

    ```lisp
    (eval-when-compile
      (require 'foo))
    ```

*   If you bind a variable in one function, and use it or set it in another function, the compiler warns about the latter function unless the variable has a definition. But adding a definition would be unclean if the variable has a short name, since Lisp packages should not define short variable names. The right thing to do is to rename this variable to start with the name prefix used for the other functions and variables in your package.

*   The last resort for avoiding a warning, when you want to do something that is usually a mistake but you know is not a mistake in your usage, is to put it inside `with-no-warnings`. See [Compiler Errors](Compiler-Errors.html).

Next: [Documentation Tips](Documentation-Tips.html), Previous: [Compilation Tips](Compilation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
