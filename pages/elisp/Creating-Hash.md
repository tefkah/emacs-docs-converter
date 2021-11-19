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

Next: [Hash Access](Hash-Access.html), Up: [Hash Tables](Hash-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 8.1 Creating Hash Tables

The principal function for creating a hash table is `make-hash-table`.

*   Function: **make-hash-table** *\&rest keyword-args*

    This function creates a new hash table according to the specified arguments. The arguments should consist of alternating keywords (particular symbols recognized specially) and values corresponding to them.

    Several keywords make sense in `make-hash-table`, but the only two that you really need to know about are `:test` and `:weakness`.

    *   `:test test`

        This specifies the method of key lookup for this hash table. The default is `eql`; `eq` and `equal` are other alternatives:

        *   `eql`

            Keys which are numbers are the same if they are `equal`, that is, if they are equal in value and either both are integers or both are floating point; otherwise, two distinct objects are never the same.

        *   `eq`

            Any two distinct Lisp objects are different as keys.

        *   `equal`

            Two Lisp objects are the same, as keys, if they are equal according to `equal`.

        You can use `define-hash-table-test` (see [Defining Hash](Defining-Hash.html)) to define additional possibilities for `test`.

    *   `:weakness weak`

        The weakness of a hash table specifies whether the presence of a key or value in the hash table preserves it from garbage collection.

        The value, `weak`, must be one of `nil`, `key`, `value`, `key-or-value`, `key-and-value`, or `t` which is an alias for `key-and-value`. If `weak` is `key` then the hash table does not prevent its keys from being collected as garbage (if they are not referenced anywhere else); if a particular key does get collected, the corresponding association is removed from the hash table.

        If `weak` is `value`, then the hash table does not prevent values from being collected as garbage (if they are not referenced anywhere else); if a particular value does get collected, the corresponding association is removed from the hash table.

        If `weak` is `key-and-value` or `t`, both the key and the value must be live in order to preserve the association. Thus, the hash table does not protect either keys or values from garbage collection; if either one is collected as garbage, that removes the association.

        If `weak` is `key-or-value`, either the key or the value can preserve the association. Thus, associations are removed from the hash table when both their key and value would be collected as garbage (if not for references from weak hash tables).

        The default for `weak` is `nil`, so that all keys and values referenced in the hash table are preserved from garbage collection.

    *   `:size size`

        This specifies a hint for how many associations you plan to store in the hash table. If you know the approximate number, you can make things a little more efficient by specifying it this way. If you specify too small a size, the hash table will grow automatically when necessary, but doing that takes some extra time.

        The default size is 65.

    *   `:rehash-size rehash-size`

        When you add an association to a hash table and the table is full, it grows automatically. This value specifies how to make the hash table larger, at that time.

        If `rehash-size` is an integer, it should be positive, and the hash table grows by adding approximately that much to the nominal size. If `rehash-size` is floating point, it had better be greater than 1, and the hash table grows by multiplying the old size by approximately that number.

        The default value is 1.5.

    *   `:rehash-threshold threshold`

        This specifies the criterion for when the hash table is full (so it should be made larger). The value, `threshold`, should be a positive floating-point number, no greater than 1. The hash table is full whenever the actual number of entries exceeds the nominal size multiplied by an approximation to this value. The default for `threshold` is 0.8125.

You can also create a new hash table using the printed representation for hash tables. The Lisp reader can read this printed representation, provided each element in the specified hash table has a valid read syntax (see [Printed Representation](Printed-Representation.html)). For instance, the following specifies a new hash table containing the keys `key1` and `key2` (both symbols) associated with `val1` (a symbol) and `300` (a number) respectively.

    #s(hash-table size 30 data (key1 val1 key2 300))

The printed representation for a hash table consists of ‘`#s`’ followed by a list beginning with ‘`hash-table`’. The rest of the list should consist of zero or more property-value pairs specifying the hash table’s properties and initial contents. The properties and values are read literally. Valid property names are `size`, `test`, `weakness`, `rehash-size`, `rehash-threshold`, and `data`. The `data` property should be a list of key-value pairs for the initial contents; the other properties have the same meanings as the matching `make-hash-table` keywords (`:size`, `:test`, etc.), described above.

Note that you cannot specify a hash table whose initial contents include objects that have no read syntax, such as buffers and frames. Such objects may be added to the hash table after it is created.

Next: [Hash Access](Hash-Access.html), Up: [Hash Tables](Hash-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
