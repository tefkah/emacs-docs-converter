

Next: [Marker Insertion Types](Marker-Insertion-Types.html), Previous: [Creating Markers](Creating-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.4 Information from Markers

This section describes the functions for accessing the components of a marker object.

*   Function: **marker-position** *marker*

    This function returns the position that `marker` points to, or `nil` if it points nowhere.

<!---->

*   Function: **marker-buffer** *marker*

    This function returns the buffer that `marker` points into, or `nil` if it points nowhere.

    ```lisp
    (setq m (make-marker))
         ⇒ #<marker in no buffer>
    ```

    ```lisp
    (marker-position m)
         ⇒ nil
    ```

    ```lisp
    (marker-buffer m)
         ⇒ nil
    ```

    ```lisp
    ```

    ```lisp
    (set-marker m 3770 (current-buffer))
         ⇒ #<marker at 3770 in markers.texi>
    ```

    ```lisp
    (marker-buffer m)
         ⇒ #<buffer markers.texi>
    ```

    ```lisp
    (marker-position m)
         ⇒ 3770
    ```
