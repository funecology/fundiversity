# Options for fundiversity

The memoisation is the convex hull computation in fundiversity is
controlled via the `fundiversity.memoise` option:

- if unset, the default is to use memoisation if memoise was installed
  when fundiversity was loaded, and not to use memoisation otherwise.

- if `options(fundiversity.memoise = TRUE)`, memoisation is used and an
  error is thrown if memoise is not installed.

- if `options(fundiversity.memoise = FALSE)`, memoisation is not used.
