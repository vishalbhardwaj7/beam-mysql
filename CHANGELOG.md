# Changelog

## 1.0.0.0

* Use `mysql-haskell` instead of `mysql`
* `Aeson.Value` instance of `FromField` more efficient and safer
* `NominalDiffTime` instance of `FromField` now handles small time differences
  properly
* Connection type no longer exported.
* `FLOAT` can only be loaded as a `Float`; analogous for `DOUBLE` and `Double`.
* Only `Text` and `ByteString` can now be loaded (not lazy, not `Char8`).
* Many unsafe conversions have been disabled.
* `TimeOfDay` and `NominalDiffTime` can now be loaded correctly.
* `OFFSET` without `LIMIT` now does nothing when requested in a query.
* `FromField` no longer requires `IO`.
* Remove URI-based connections
* Remove `runInsertRowReturning` (should be unnecessary)
