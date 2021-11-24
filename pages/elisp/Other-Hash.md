

### 8.4 Other Hash Table Functions

Here are some other functions for working with hash tables.

### Function: **hash-table-p** *table*

This returns non-`nil` if `table` is a hash table object.

### Function: **copy-hash-table** *table*

This function creates and returns a copy of `table`. Only the table itself is copied—the keys and values are shared.

### Function: **hash-table-count** *table*

This function returns the actual number of entries in `table`.

### Function: **hash-table-test** *table*

This returns the `test` value that was given when `table` was created, to specify how to hash and compare keys. See `make-hash-table` (see [Creating Hash](Creating-Hash.html)).

### Function: **hash-table-weakness** *table*

This function returns the `weak` value that was specified for hash table `table`.

### Function: **hash-table-rehash-size** *table*

This returns the rehash size of `table`.

### Function: **hash-table-rehash-threshold** *table*

This returns the rehash threshold of `table`.

### Function: **hash-table-size** *table*

This returns the current nominal size of `table`.
