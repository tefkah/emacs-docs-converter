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

Next: [Arrays](Arrays.html), Up: [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 6.1 Sequences

This section describes functions that accept any kind of sequence.

*   Function: **sequencep** *object*

    This function returns `t` if `object` is a list, vector, string, bool-vector, or char-table, `nil` otherwise. See also `seqp` below.

<!---->

*   Function: **length** *sequence*

    This function returns the number of elements in `sequence`. The function signals the `wrong-type-argument` error if the argument is not a sequence or is a dotted list; it signals the `circular-list` error if the argument is a circular list. For a char-table, the value returned is always one more than the maximum Emacs character code.

    See [Definition of safe-length](List-Elements.html#Definition-of-safe_002dlength), for the related function `safe-length`.

        (length '(1 2 3))
            ⇒ 3

    <!---->

        (length ())
            ⇒ 0

    <!---->

        (length "foobar")
            ⇒ 6

    <!---->

        (length [1 2 3])
            ⇒ 3

    <!---->

        (length (make-bool-vector 5 nil))
            ⇒ 5

See also `string-bytes`, in [Text Representations](Text-Representations.html).

If you need to compute the width of a string on display, you should use `string-width` (see [Size of Displayed Text](Size-of-Displayed-Text.html)), not `length`, since `length` only counts the number of characters, but does not account for the display width of each character.

*   Function: **elt** *sequence index*

    This function returns the element of `sequence` indexed by `index`. Legitimate values of `index` are integers ranging from 0 up to one less than the length of `sequence`. If `sequence` is a list, out-of-range values behave as for `nth`. See [Definition of nth](List-Elements.html#Definition-of-nth). Otherwise, out-of-range values trigger an `args-out-of-range` error.

        (elt [1 2 3 4] 2)
             ⇒ 3

    <!---->

        (elt '(1 2 3 4) 2)
             ⇒ 3

    <!---->

        ;; We use string to show clearly which character elt returns.
        (string (elt "1234" 2))
             ⇒ "3"

    <!---->

        (elt [1 2 3 4] 4)
             error→ Args out of range: [1 2 3 4], 4

    <!---->

        (elt [1 2 3 4] -1)
             error→ Args out of range: [1 2 3 4], -1

    This function generalizes `aref` (see [Array Functions](Array-Functions.html)) and `nth` (see [Definition of nth](List-Elements.html#Definition-of-nth)).

<!---->

*   Function: **copy-sequence** *seqr*

    This function returns a copy of `seqr`, which should be either a sequence or a record. The copy is the same type of object as the original, and it has the same elements in the same order. However, if `seqr` is empty, like a string or a vector of zero length, the value returned by this function might not be a copy, but an empty object of the same type and identical to `seqr`.

    Storing a new element into the copy does not affect the original `seqr`, and vice versa. However, the elements of the copy are not copies; they are identical (`eq`) to the elements of the original. Therefore, changes made within these elements, as found via the copy, are also visible in the original.

    If the argument is a string with text properties, the property list in the copy is itself a copy, not shared with the original’s property list. However, the actual values of the properties are shared. See [Text Properties](Text-Properties.html).

    This function does not work for dotted lists. Trying to copy a circular list may cause an infinite loop.

    See also `append` in [Building Lists](Building-Lists.html), `concat` in [Creating Strings](Creating-Strings.html), and `vconcat` in [Vector Functions](Vector-Functions.html), for other ways to copy sequences.

        (setq bar (list 1 2))
             ⇒ (1 2)

    <!---->

        (setq x (vector 'foo bar))
             ⇒ [foo (1 2)]

    <!---->

        (setq y (copy-sequence x))
             ⇒ [foo (1 2)]

    ```
    ```

        (eq x y)
             ⇒ nil

    <!---->

        (equal x y)
             ⇒ t

    <!---->

        (eq (elt x 1) (elt y 1))
             ⇒ t

    ```
    ```

        ;; Replacing an element of one sequence.
        (aset x 0 'quux)
        x ⇒ [quux (1 2)]
        y ⇒ [foo (1 2)]

    ```
    ```

        ;; Modifying the inside of a shared element.
        (setcar (aref x 1) 69)
        x ⇒ [quux (69 2)]
        y ⇒ [foo (69 2)]

<!---->

*   Function: **reverse** *sequence*

    This function creates a new sequence whose elements are the elements of `sequence`, but in reverse order. The original argument `sequence` is *not* altered. Note that char-tables cannot be reversed.

        (setq x '(1 2 3 4))
             ⇒ (1 2 3 4)

    <!---->

        (reverse x)
             ⇒ (4 3 2 1)
        x
             ⇒ (1 2 3 4)

    <!---->

        (setq x [1 2 3 4])
             ⇒ [1 2 3 4]

    <!---->

        (reverse x)
             ⇒ [4 3 2 1]
        x
             ⇒ [1 2 3 4]

    <!---->

        (setq x "xyzzy")
             ⇒ "xyzzy"

    <!---->

        (reverse x)
             ⇒ "yzzyx"
        x
             ⇒ "xyzzy"

<!---->

*   Function: **nreverse** *sequence*

    This function reverses the order of the elements of `sequence`. Unlike `reverse` the original `sequence` may be modified.

    For example:

        (setq x (list 'a 'b 'c))
             ⇒ (a b c)

    <!---->

        x
             ⇒ (a b c)
        (nreverse x)
             ⇒ (c b a)

    <!---->

        ;; The cons cell that was first is now last.
        x
             ⇒ (a)

    To avoid confusion, we usually store the result of `nreverse` back in the same variable which held the original list:

        (setq x (nreverse x))

    Here is the `nreverse` of our favorite example, `(a b c)`, presented graphically:

        Original list head:                       Reversed list:
         -------------        -------------        ------------
        | car  | cdr  |      | car  | cdr  |      | car | cdr  |
        |   a  |  nil |<--   |   b  |   o  |<--   |   c |   o  |
        |      |      |   |  |      |   |  |   |  |     |   |  |
         -------------    |   --------- | -    |   -------- | -
                          |             |      |            |
                           -------------        ------------

    For the vector, it is even simpler because you don’t need setq:

        (setq x (copy-sequence [1 2 3 4]))
             ⇒ [1 2 3 4]
        (nreverse x)
             ⇒ [4 3 2 1]
        x
             ⇒ [4 3 2 1]

    Note that unlike `reverse`, this function doesn’t work with strings. Although you can alter string data by using `aset`, it is strongly encouraged to treat strings as immutable even when they are mutable. See [Mutability](Mutability.html).

<!---->

*   Function: **sort** *sequence predicate*

    This function sorts `sequence` stably. Note that this function doesn’t work for all sequences; it may be used only for lists and vectors. If `sequence` is a list, it is modified destructively. This functions returns the sorted `sequence` and compares elements using `predicate`. A stable sort is one in which elements with equal sort keys maintain their relative order before and after the sort. Stability is important when successive sorts are used to order elements according to different criteria.

    The argument `predicate` must be a function that accepts two arguments. It is called with two elements of `sequence`. To get an increasing order sort, the `predicate` should return non-`nil` if the first element is “less” than the second, or `nil` if not.

    The comparison function `predicate` must give reliable results for any given pair of arguments, at least within a single call to `sort`. It must be *antisymmetric*; that is, if `a` is less than `b`, `b` must not be less than `a`. It must be *transitive*—that is, if `a` is less than `b`, and `b` is less than `c`, then `a` must be less than `c`. If you use a comparison function which does not meet these requirements, the result of `sort` is unpredictable.

    The destructive aspect of `sort` for lists is that it rearranges the cons cells forming `sequence` by changing CDRs. A nondestructive sort function would create new cons cells to store the elements in their sorted order. If you wish to make a sorted copy without destroying the original, copy it first with `copy-sequence` and then sort.

    Sorting does not change the CARs of the cons cells in `sequence`; the cons cell that originally contained the element `a` in `sequence` still has `a` in its CAR after sorting, but it now appears in a different position in the list due to the change of CDRs. For example:

        (setq nums (list 1 3 2 6 5 4 0))
             ⇒ (1 3 2 6 5 4 0)

    <!---->

        (sort nums #'<)
             ⇒ (0 1 2 3 4 5 6)

    <!---->

        nums
             ⇒ (1 2 3 4 5 6)

    **Warning**: Note that the list in `nums` no longer contains 0; this is the same cons cell that it was before, but it is no longer the first one in the list. Don’t assume a variable that formerly held the argument now holds the entire sorted list! Instead, save the result of `sort` and use that. Most often we store the result back into the variable that held the original list:

        (setq nums (sort nums #'<))

    For the better understanding of what stable sort is, consider the following vector example. After sorting, all items whose `car` is 8 are grouped at the beginning of `vector`, but their relative order is preserved. All items whose `car` is 9 are grouped at the end of `vector`, but their relative order is also preserved:

        (setq
          vector
          (vector '(8 . "xxx") '(9 . "aaa") '(8 . "bbb") '(9 . "zzz")
                  '(9 . "ppp") '(8 . "ttt") '(8 . "eee") '(9 . "fff")))
             ⇒ [(8 . "xxx") (9 . "aaa") (8 . "bbb") (9 . "zzz")
                 (9 . "ppp") (8 . "ttt") (8 . "eee") (9 . "fff")]

    <!---->

        (sort vector (lambda (x y) (< (car x) (car y))))
             ⇒ [(8 . "xxx") (8 . "bbb") (8 . "ttt") (8 . "eee")
                 (9 . "aaa") (9 . "zzz") (9 . "ppp") (9 . "fff")]

    See [Sorting](Sorting.html), for more functions that perform sorting. See `documentation` in [Accessing Documentation](Accessing-Documentation.html), for a useful example of `sort`.

The `seq.el` library provides the following additional sequence manipulation macros and functions, prefixed with `seq-`. To use them, you must first load the `seq` library.

All functions defined in this library are free of side-effects; i.e., they do not modify any sequence (list, vector, or string) that you pass as an argument. Unless otherwise stated, the result is a sequence of the same type as the input. For those functions that take a predicate, this should be a function of one argument.

The `seq.el` library can be extended to work with additional types of sequential data-structures. For that purpose, all functions are defined using `cl-defgeneric`. See [Generic Functions](Generic-Functions.html), for more details about using `cl-defgeneric` for adding extensions.

*   Function: **seq-elt** *sequence index*

    This function returns the element of `sequence` at the specified `index`, which is an integer whose valid value range is zero to one less than the length of `sequence`. For out-of-range values on built-in sequence types, `seq-elt` behaves like `elt`. For the details, see [Definition of elt](#Definition-of-elt).

        (seq-elt [1 2 3 4] 2)
        ⇒ 3

    `seq-elt` returns places settable using `setf` (see [Setting Generalized Variables](Setting-Generalized-Variables.html)).

        (setq vec [1 2 3 4])
        (setf (seq-elt vec 2) 5)
        vec
        ⇒ [1 2 5 4]

<!---->

*   Function: **seq-length** *sequence*

    This function returns the number of elements in `sequence`. For built-in sequence types, `seq-length` behaves like `length`. See [Definition of length](#Definition-of-length).

<!---->

*   Function: **seqp** *object*

    This function returns non-`nil` if `object` is a sequence (a list or array), or any additional type of sequence defined via `seq.el` generic functions. This is an extensible variant of `sequencep`.

        (seqp [1 2])
        ⇒ t

    <!---->

        (seqp 2)
        ⇒ nil

<!---->

*   Function: **seq-drop** *sequence n*

    This function returns all but the first `n` (an integer) elements of `sequence`. If `n` is negative or zero, the result is `sequence`.

        (seq-drop [1 2 3 4 5 6] 3)
        ⇒ [4 5 6]

    <!---->

        (seq-drop "hello world" -4)
        ⇒ "hello world"

<!---->

*   Function: **seq-take** *sequence n*

    This function returns the first `n` (an integer) elements of `sequence`. If `n` is negative or zero, the result is `nil`.

        (seq-take '(1 2 3 4) 3)
        ⇒ (1 2 3)

    <!---->

        (seq-take [1 2 3 4] 0)
        ⇒ []

<!---->

*   Function: **seq-take-while** *predicate sequence*

    This function returns the members of `sequence` in order, stopping before the first one for which `predicate` returns `nil`.

        (seq-take-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2))
        ⇒ (1 2 3)

    <!---->

        (seq-take-while (lambda (elt) (> elt 0)) [-1 4 6])
        ⇒ []

<!---->

*   Function: **seq-drop-while** *predicate sequence*

    This function returns the members of `sequence` in order, starting from the first one for which `predicate` returns `nil`.

        (seq-drop-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2))
        ⇒ (-1 -2)

    <!---->

        (seq-drop-while (lambda (elt) (< elt 0)) [1 4 6])
        ⇒ [1 4 6]

<!---->

*   Function: **seq-do** *function sequence*

    This function applies `function` to each element of `sequence` in turn (presumably for side effects), and returns `sequence`.

<!---->

*   Function: **seq-map** *function sequence*

    This function returns the result of applying `function` to each element of `sequence`. The returned value is a list.

        (seq-map #'1+ '(2 4 6))
        ⇒ (3 5 7)

    <!---->

        (seq-map #'symbol-name [foo bar])
        ⇒ ("foo" "bar")

<!---->

*   Function: **seq-map-indexed** *function sequence*

    This function returns the result of applying `function` to each element of `sequence` and its index within `seq`. The returned value is a list.

        (seq-map-indexed (lambda (elt idx)
                           (list idx elt))
                         '(a b c))
        ⇒ ((0 a) (1 b) (2 c))

<!---->

*   Function: **seq-mapn** *function \&rest sequences*

    This function returns the result of applying `function` to each element of `sequences`. The arity (see [subr-arity](What-Is-a-Function.html)) of `function` must match the number of sequences. Mapping stops at the end of the shortest sequence, and the returned value is a list.

        (seq-mapn #'+ '(2 4 6) '(20 40 60))
        ⇒ (22 44 66)

    <!---->

        (seq-mapn #'concat '("moskito" "bite") ["bee" "sting"])
        ⇒ ("moskitobee" "bitesting")

<!---->

*   Function: **seq-filter** *predicate sequence*

    This function returns a list of all the elements in `sequence` for which `predicate` returns non-`nil`.

        (seq-filter (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
        ⇒ (1 3 5)

    <!---->

        (seq-filter (lambda (elt) (> elt 0)) '(-1 -3 -5))
        ⇒ nil

<!---->

*   Function: **seq-remove** *predicate sequence*

    This function returns a list of all the elements in `sequence` for which `predicate` returns `nil`.

        (seq-remove (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
        ⇒ (-1 -3)

    <!---->

        (seq-remove (lambda (elt) (< elt 0)) '(-1 -3 -5))
        ⇒ nil

<!---->

*   Function: **seq-reduce** *function sequence initial-value*

    This function returns the result of calling `function` with `initial-value` and the first element of `sequence`, then calling `function` with that result and the second element of `sequence`, then with that result and the third element of `sequence`, etc. `function` should be a function of two arguments.

    `function` is called with two arguments. `intial-value` (and then the accumulated value) is used as the first argument, and the elements in `sequence` are used for the second argument.

    If `sequence` is empty, this returns `initial-value` without calling `function`.

        (seq-reduce #'+ [1 2 3 4] 0)
        ⇒ 10

    <!---->

        (seq-reduce #'+ '(1 2 3 4) 5)
        ⇒ 15

    <!---->

        (seq-reduce #'+ '() 3)
        ⇒ 3

<!---->

*   Function: **seq-some** *predicate sequence*

    This function returns the first non-`nil` value returned by applying `predicate` to each element of `sequence` in turn.

        (seq-some #'numberp ["abc" 1 nil])
        ⇒ t

    <!---->

        (seq-some #'numberp ["abc" "def"])
        ⇒ nil

    <!---->

        (seq-some #'null ["abc" 1 nil])
        ⇒ t

    <!---->

        (seq-some #'1+ [2 4 6])
        ⇒ 3

<!---->

*   Function: **seq-find** *predicate sequence \&optional default*

    This function returns the first element in `sequence` for which `predicate` returns non-`nil`. If no element matches `predicate`, the function returns `default`.

    Note that this function has an ambiguity if the found element is identical to `default`, as in that case it cannot be known whether an element was found or not.

        (seq-find #'numberp ["abc" 1 nil])
        ⇒ 1

    <!---->

        (seq-find #'numberp ["abc" "def"])
        ⇒ nil

<!---->

*   Function: **seq-every-p** *predicate sequence*

    This function returns non-`nil` if applying `predicate` to every element of `sequence` returns non-`nil`.

        (seq-every-p #'numberp [2 4 6])
        ⇒ t

    <!---->

        (seq-every-p #'numberp [2 4 "6"])
        ⇒ nil

<!---->

*   Function: **seq-empty-p** *sequence*

    This function returns non-`nil` if `sequence` is empty.

        (seq-empty-p "not empty")
        ⇒ nil

    <!---->

        (seq-empty-p "")
        ⇒ t

<!---->

*   Function: **seq-count** *predicate sequence*

    This function returns the number of elements in `sequence` for which `predicate` returns non-`nil`.

        (seq-count (lambda (elt) (> elt 0)) [-1 2 0 3 -2])
        ⇒ 2

<!---->

*   Function: **seq-sort** *function sequence*

    This function returns a copy of `sequence` that is sorted according to `function`, a function of two arguments that returns non-`nil` if the first argument should sort before the second.

<!---->

*   Function: **seq-sort-by** *function predicate sequence*

    This function is similar to `seq-sort`, but the elements of `sequence` are transformed by applying `function` on them before being sorted. `function` is a function of one argument.

        (seq-sort-by #'seq-length #'> ["a" "ab" "abc"])
        ⇒ ["abc" "ab" "a"]

<!---->

*   Function: **seq-contains-p** *sequence elt \&optional function*

    This function returns non-`nil` if at least one element in `sequence` is equal to `elt`. If the optional argument `function` is non-`nil`, it is a function of two arguments to use instead of the default `equal`.

        (seq-contains-p '(symbol1 symbol2) 'symbol1)
        ⇒ t

    <!---->

        (seq-contains-p '(symbol1 symbol2) 'symbol3)
        ⇒ nil

<!---->

*   Function: **seq-set-equal-p** *sequence1 sequence2 \&optional testfn*

    This function checks whether `sequence1` and `sequence2` contain the same elements, regardless of the order. If the optional argument `testfn` is non-`nil`, it is a function of two arguments to use instead of the default `equal`.

        (seq-set-equal-p '(a b c) '(c b a))
        ⇒ t

    <!---->

        (seq-set-equal-p '(a b c) '(c b))
        ⇒ nil

    <!---->

        (seq-set-equal-p '("a" "b" "c") '("c" "b" "a"))
        ⇒ t

    <!---->

        (seq-set-equal-p '("a" "b" "c") '("c" "b" "a") #'eq)
        ⇒ nil

<!---->

*   Function: **seq-position** *sequence elt \&optional function*

    This function returns the index of the first element in `sequence` that is equal to `elt`. If the optional argument `function` is non-`nil`, it is a function of two arguments to use instead of the default `equal`.

        (seq-position '(a b c) 'b)
        ⇒ 1

    <!---->

        (seq-position '(a b c) 'd)
        ⇒ nil

<!---->

*   Function: **seq-uniq** *sequence \&optional function*

    This function returns a list of the elements of `sequence` with duplicates removed. If the optional argument `function` is non-`nil`, it is a function of two arguments to use instead of the default `equal`.

        (seq-uniq '(1 2 2 1 3))
        ⇒ (1 2 3)

    <!---->

        (seq-uniq '(1 2 2.0 1.0) #'=)
        ⇒ (1 2)

<!---->

*   Function: **seq-subseq** *sequence start \&optional end*

    This function returns a subset of `sequence` from `start` to `end`, both integers (`end` defaults to the last element). If `start` or `end` is negative, it counts from the end of `sequence`.

        (seq-subseq '(1 2 3 4 5) 1)
        ⇒ (2 3 4 5)

    <!---->

        (seq-subseq '[1 2 3 4 5] 1 3)
        ⇒ [2 3]

    <!---->

        (seq-subseq '[1 2 3 4 5] -3 -1)
        ⇒ [3 4]

<!---->

*   Function: **seq-concatenate** *type \&rest sequences*

    This function returns a sequence of type `type` made of the concatenation of `sequences`. `type` may be: `vector`, `list` or `string`.

        (seq-concatenate 'list '(1 2) '(3 4) [5 6])
        ⇒ (1 2 3 4 5 6)

    <!---->

        (seq-concatenate 'string "Hello " "world")
        ⇒ "Hello world"

<!---->

*   Function: **seq-mapcat** *function sequence \&optional type*

    This function returns the result of applying `seq-concatenate` to the result of applying `function` to each element of `sequence`. The result is a sequence of type `type`, or a list if `type` is `nil`.

        (seq-mapcat #'seq-reverse '((3 2 1) (6 5 4)))
        ⇒ (1 2 3 4 5 6)

<!---->

*   Function: **seq-partition** *sequence n*

    This function returns a list of the elements of `sequence` grouped into sub-sequences of length `n`. The last sequence may contain less elements than `n`. `n` must be an integer. If `n` is a negative integer or 0, the return value is `nil`.

        (seq-partition '(0 1 2 3 4 5 6 7) 3)
        ⇒ ((0 1 2) (3 4 5) (6 7))

<!---->

*   Function: **seq-intersection** *sequence1 sequence2 \&optional function*

    This function returns a list of the elements that appear both in `sequence1` and `sequence2`. If the optional argument `function` is non-`nil`, it is a function of two arguments to use to compare elements instead of the default `equal`.

        (seq-intersection [2 3 4 5] [1 3 5 6 7])
        ⇒ (3 5)

<!---->

*   Function: **seq-difference** *sequence1 sequence2 \&optional function*

    This function returns a list of the elements that appear in `sequence1` but not in `sequence2`. If the optional argument `function` is non-`nil`, it is a function of two arguments to use to compare elements instead of the default `equal`.

        (seq-difference '(2 3 4 5) [1 3 5 6 7])
        ⇒ (2 4)

<!---->

*   Function: **seq-group-by** *function sequence*

    This function separates the elements of `sequence` into an alist whose keys are the result of applying `function` to each element of `sequence`. Keys are compared using `equal`.

        (seq-group-by #'integerp '(1 2.1 3 2 3.2))
        ⇒ ((t 1 3 2) (nil 2.1 3.2))

    <!---->

        (seq-group-by #'car '((a 1) (b 2) (a 3) (c 4)))
        ⇒ ((b (b 2)) (a (a 1) (a 3)) (c (c 4)))

<!---->

*   Function: **seq-into** *sequence type*

    This function converts the sequence `sequence` into a sequence of type `type`. `type` can be one of the following symbols: `vector`, `string` or `list`.

        (seq-into [1 2 3] 'list)
        ⇒ (1 2 3)

    <!---->

        (seq-into nil 'vector)
        ⇒ []

    <!---->

        (seq-into "hello" 'vector)
        ⇒ [104 101 108 108 111]

<!---->

*   Function: **seq-min** *sequence*

    This function returns the smallest element of `sequence`. The elements of `sequence` must be numbers or markers (see [Markers](Markers.html)).

        (seq-min [3 1 2])
        ⇒ 1

    <!---->

        (seq-min "Hello")
        ⇒ 72

<!---->

*   Function: **seq-max** *sequence*

    This function returns the largest element of `sequence`. The elements of `sequence` must be numbers or markers.

        (seq-max [1 3 2])
        ⇒ 3

    <!---->

        (seq-max "Hello")
        ⇒ 111

<!---->

*   Macro: **seq-doseq** *(var sequence) body…*

    This macro is like `dolist` (see [dolist](Iteration.html)), except that `sequence` can be a list, vector or string. This is primarily useful for side-effects.

<!---->

*   Macro: **seq-let** *var-sequence val-sequence body…*

    This macro binds the variables defined in `var-sequence` to the values that are the corresponding elements of `val-sequence`. This is known as *destructuring binding*. The elements of `var-sequence` can themselves include sequences, allowing for nested destructuring.

    The `var-sequence` sequence can also include the `&rest` marker followed by a variable name to be bound to the rest of `val-sequence`.

        (seq-let [first second] [1 2 3 4]
          (list first second))
        ⇒ (1 2)

    <!---->

        (seq-let (_ a _ b) '(1 2 3 4)
          (list a b))
        ⇒ (2 4)

    <!---->

        (seq-let [a [b [c]]] [1 [2 [3]]]
          (list a b c))
        ⇒ (1 2 3)

    <!---->

        (seq-let [a b &rest others] [1 2 3 4]
          others)

    <!---->

        ⇒ [3 4]

    The `pcase` patterns provide an alternative facility for destructuring binding, see [Destructuring with pcase Patterns](Destructuring-with-pcase-Patterns.html).

<!---->

*   Function: **seq-random-elt** *sequence*

    This function returns an element of `sequence` taken at random.

        (seq-random-elt [1 2 3 4])
        ⇒ 3
        (seq-random-elt [1 2 3 4])
        ⇒ 2
        (seq-random-elt [1 2 3 4])
        ⇒ 4
        (seq-random-elt [1 2 3 4])
        ⇒ 2
        (seq-random-elt [1 2 3 4])
        ⇒ 1

    If `sequence` is empty, this function signals an error.

Next: [Arrays](Arrays.html), Up: [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
