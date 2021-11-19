

Next: [Timers](Timers.html), Previous: [Processor Run Time](Processor-Run-Time.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 40.10 Time Calculations

These functions perform calendrical computations using time values (see [Time of Day](Time-of-Day.html)). As with any time value, a value of `nil` for any of their time-value arguments stands for the current system time, and a single number stands for the number of seconds since the epoch.

*   Function: **time-less-p** *t1 t2*

    This returns `t` if time value `t1` is less than time value `t2`. The result is `nil` if either argument is a NaN.

<!---->

*   Function: **time-equal-p** *t1 t2*

    This returns `t` if `t1` and `t2` are equal time values. The result is `nil` if either argument is a NaN.

<!---->

*   Function: **time-subtract** *t1 t2*

    This returns the time difference `t1` - `t2` between two time values, as a Lisp time value. The result is exact and its clock resolution is no worse than the worse of its two arguments’ resolutions. The result is floating-point only if it is infinite or a NaN. If you need the difference in units of elapsed seconds, you can convert it with `time-convert` or `float-time`. See [Time Conversion](Time-Conversion.html).

<!---->

*   Function: **time-add** *t1 t2*

    This returns the sum of two time values, using the same conversion rules as `time-subtract`. One argument should represent a time difference rather than a point in time, as a time value that is often just a single number of elapsed seconds. Here is how to add a number of seconds to a time value:

    ```lisp
    (time-add time seconds)
    ```

<!---->

*   Function: **time-to-days** *time-value*

    This function returns the number of days between the beginning of year 1 and `time-value`, assuming the default time zone. The operating system limits the range of time and zone values.

<!---->

*   Function: **time-to-day-in-year** *time-value*

    This returns the day number within the year corresponding to `time-value`, assuming the default time zone. The operating system limits the range of time and zone values.

<!---->

*   Function: **date-leap-year-p** *year*

    This function returns `t` if `year` is a leap year.

<!---->

*   Function: **date-days-in-month** *year month*

    Return the number of days in `month` in `year`. For instance, February 2020 has 29 days.

<!---->

*   Function: **date-ordinal-to-time** *year ordinal*

    Return the date of `ordinal` in `year` as a decoded time structure. For instance, the 120th day in 2004 is April 29th.

Next: [Timers](Timers.html), Previous: [Processor Run Time](Processor-Run-Time.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
