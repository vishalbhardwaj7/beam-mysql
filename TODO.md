# TODO

* `toSQLTypeName` is currently brittle, as it treats every type other than the
  ones it explicitly recognizes as `Geometry`. This is largely due to
  `mysql-haskell` not fully supporting every type available in MySQL. This
  should be fixed to something less brittle (possibly upstream).
* The `FromField` instance for `TimeOfDay` currently signals a `TypeMismatch'`
  when given a non-zero first field. This is uninformative, possibly requiring a
  special error of its own.
* In `runInsertRowReturning`, we need to perform a check against an
  auto-incrementing field for its value. The current solution is gory - is there
  a better way?
