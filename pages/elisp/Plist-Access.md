

#### 5.9.2 Property Lists Outside Symbols

The following functions can be used to manipulate property lists. They all compare property names using `eq`.

### Function: **plist-get** *plist property*

This returns the value of the `property` property stored in the property list `plist`. It accepts a malformed `plist` argument. If `property` is not found in the `plist`, it returns `nil`. For example,

```lisp
(plist-get '(foo 4) 'foo)
     ⇒ 4
(plist-get '(foo 4 bad) 'foo)
     ⇒ 4
(plist-get '(foo 4 bad) 'bad)
     ⇒ nil
(plist-get '(foo 4 bad) 'bar)
     ⇒ nil
```

### Function: **plist-put** *plist property value*

This stores `value` as the value of the `property` property in the property list `plist`. It may modify `plist` destructively, or it may construct a new list structure without altering the old. The function returns the modified property list, so you can store that back in the place where you got `plist`. For example,

```lisp
(setq my-plist (list 'bar t 'foo 4))
     ⇒ (bar t foo 4)
(setq my-plist (plist-put my-plist 'foo 69))
     ⇒ (bar t foo 69)
(setq my-plist (plist-put my-plist 'quux '(a)))
     ⇒ (bar t foo 69 quux (a))
```

### Function: **lax-plist-get** *plist property*

Like `plist-get` except that it compares properties using `equal` instead of `eq`.

### Function: **lax-plist-put** *plist property value*

Like `plist-put` except that it compares properties using `equal` instead of `eq`.

### Function: **plist-member** *plist property*

This returns non-`nil` if `plist` contains the given `property`. Unlike `plist-get`, this allows you to distinguish between a missing property and a property with the value `nil`. The value is actually the tail of `plist` whose `car` is `property`.
