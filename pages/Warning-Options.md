

Next: [Delayed Warnings](Delayed-Warnings.html), Previous: [Warning Variables](Warning-Variables.html), Up: [Warnings](Warnings.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.5.3 Warning Options

These variables are used by users to control what happens when a Lisp program reports a warning.

*   User Option: **warning-minimum-level**

    This user option specifies the minimum severity level that should be shown immediately to the user. The default is `:warning`, which means to immediately display all warnings except `:debug` warnings.

<!---->

*   User Option: **warning-minimum-log-level**

    This user option specifies the minimum severity level that should be logged in the warnings buffer. The default is `:warning`, which means to log all warnings except `:debug` warnings.

<!---->

*   User Option: **warning-suppress-types**

    This list specifies which warning types should not be displayed immediately for the user. Each element of the list should be a list of symbols. If its elements match the first elements in a warning type, then that warning is not displayed immediately.

<!---->

*   User Option: **warning-suppress-log-types**

    This list specifies which warning types should not be logged in the warnings buffer. Each element of the list should be a list of symbols. If it matches the first few elements in a warning type, then that warning is not logged.
