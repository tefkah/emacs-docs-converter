

Next: [Replacing](Replacing.html), Previous: [Registers](Registers.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.22 Transposition of Text

This function can be used to transpose stretches of text:

*   Function: **transpose-regions** *start1 end1 start2 end2 \&optional leave-markers*

    This function exchanges two nonoverlapping portions of the buffer (if they overlap, the function signals an error). Arguments `start1` and `end1` specify the bounds of one portion and arguments `start2` and `end2` specify the bounds of the other portion.

    Normally, `transpose-regions` relocates markers with the transposed text; a marker previously positioned within one of the two transposed portions moves along with that portion, thus remaining between the same two characters in their new position. However, if `leave-markers` is non-`nil`, `transpose-regions` does not do this—it leaves all markers unrelocated.
