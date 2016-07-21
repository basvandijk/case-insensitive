[![Hackage](https://img.shields.io/hackage/v/case-insensitive.svg)](https://hackage.haskell.org/package/case-insensitive)
[![Build Status](https://travis-ci.org/basvandijk/case-insensitive.svg)](https://travis-ci.org/basvandijk/case-insensitive)

The module `Data.CaseInsensitive` provides the `CI` type constructor
which can be parameterised by a string-like type like: `String`,
`ByteString`, `Text`, etc.. Comparisons of values of the resulting
type will be insensitive to cases.
