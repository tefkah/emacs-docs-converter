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

Next: [Errors](Errors.html), Previous: [Catch and Throw](Catch-and-Throw.html), Up: [Nonlocal Exits](Nonlocal-Exits.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.2 Examples of `catch` and `throw`

One way to use `catch` and `throw` is to exit from a doubly nested loop. (In most languages, this would be done with a `goto`.) Here we compute `(foo i j)` for `i` and `j` varying from 0 to 9:

    (defun search-foo ()
      (catch 'loop
        (let ((i 0))
          (while (< i 10)
            (let ((j 0))
              (while (< j 10)
                (if (foo i j)
                    (throw 'loop (list i j)))
                (setq j (1+ j))))
            (setq i (1+ i))))))

If `foo` ever returns non-`nil`, we stop immediately and return a list of `i` and `j`. If `foo` always returns `nil`, the `catch` returns normally, and the value is `nil`, since that is the result of the `while`.

Here are two tricky examples, slightly different, showing two return points at once. First, two return points with the same tag, `hack`:

    (defun catch2 (tag)
      (catch tag
        (throw 'hack 'yes)))
    ⇒ catch2

```
```

    (catch 'hack
      (print (catch2 'hack))
      'no)
    -| yes
    ⇒ no

Since both return points have tags that match the `throw`, it goes to the inner one, the one established in `catch2`. Therefore, `catch2` returns normally with value `yes`, and this value is printed. Finally the second body form in the outer `catch`, which is `'no`, is evaluated and returned from the outer `catch`.

Now let’s change the argument given to `catch2`:

    (catch 'hack
      (print (catch2 'quux))
      'no)
    ⇒ yes

We still have two return points, but this time only the outer one has the tag `hack`; the inner one has the tag `quux` instead. Therefore, `throw` makes the outer `catch` return the value `yes`. The function `print` is never called, and the body-form `'no` is never evaluated.

Next: [Errors](Errors.html), Previous: [Catch and Throw](Catch-and-Throw.html), Up: [Nonlocal Exits](Nonlocal-Exits.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
