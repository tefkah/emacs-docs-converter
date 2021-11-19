

Next: [Mutex Type](Mutex-Type.html), Previous: [Process Type](Process-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.5.9 Thread Type

A *thread* in Emacs represents a separate thread of Emacs Lisp execution. It runs its own Lisp program, has its own current buffer, and can have subprocesses locked to it, i.e. subprocesses whose output only this thread can accept. See [Threads](Threads.html).

Thread objects have no read syntax. They print in hash notation, giving the name of the thread (if it has been given a name) or its address in core:

```lisp
(all-threads)
    ⇒ (#<thread 0176fc40>)
```
