# Changelog

## 1.0.0.0

* Use `mysql-haskell` instead of `mysql`
* Be safer when extracting `Int` or `Word` fields
* Remove `ByteString.Char8` instance of `FromField`
* Remove `Text.Lazy` instance of `FromField`
* `Text` and both kinds of `ByteString` instances of `FromField` now more efficient
* `Aeson.Value` instance of `FromField` more efficient and safer
* `NominalDiffTime` instance of `FromField` now handles small time differences
  properly
