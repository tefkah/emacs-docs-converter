

Next: [Backward Compatibility](Backward-Compatibility.html), Up: [Records](Records.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 7.1 Record Functions

*   Function: **recordp** *object*

    This function returns `t` if `object` is a record.

    ```lisp
    (recordp #s(a))
         ⇒ t
    ```

<!---->

*   Function: **record** *type \&rest objects*

    This function creates and returns a record whose type is `type` and remaining slots are the rest of the arguments, `objects`.

    ```lisp
    (record 'foo 23 [bar baz] "rats")
         ⇒ #s(foo 23 [bar baz] "rats")
    ```

<!---->

*   Function: **make-record** *type length object*

    This function returns a new record with type `type` and `length` more slots, each initialized to `object`.

    ```lisp
    (setq sleepy (make-record 'foo 9 'Z))
         ⇒ #s(foo Z Z Z Z Z Z Z Z Z)
    ```
