

Next: [Search and Replace](Search-and-Replace.html), Previous: [POSIX Regexps](POSIX-Regexps.html), Up: [Searching and Matching](Searching-and-Matching.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 34.6 The Match Data

Emacs keeps track of the start and end positions of the segments of text found during a search; this is called the *match data*. Thanks to the match data, you can search for a complex pattern, such as a date in a mail message, and then extract parts of the match under control of the pattern.

Because the match data normally describe the most recent search only, you must be careful not to do another search inadvertently between the search you wish to refer back to and the use of the match data. If you can’t avoid another intervening search, you must save and restore the match data around it, to prevent it from being overwritten.

Notice that all functions are allowed to overwrite the match data unless they’re explicitly documented not to do so. A consequence is that functions that are run implicitly in the background (see [Timers](Timers.html), and [Idle Timers](Idle-Timers.html)) should likely save and restore the match data explicitly.

|                                               |    |                                                                                         |
| :-------------------------------------------- | -- | :-------------------------------------------------------------------------------------- |
| • [Replacing Match](Replacing-Match.html)     |    | Replacing a substring that was matched.                                                 |
| • [Simple Match Data](Simple-Match-Data.html) |    | Accessing single items of match data, such as where a particular subexpression started. |
| • [Entire Match Data](Entire-Match-Data.html) |    | Accessing the entire match data at once, as a list.                                     |
| • [Saving Match Data](Saving-Match-Data.html) |    | Saving and restoring the match data.                                                    |
