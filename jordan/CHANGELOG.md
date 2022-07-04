# Revision history for jordan

## 0.2.0.0 - 2022-07-04

* Removed support for serializing to `Text`.
* Removed support for parsing via Megaparsec.
* Added support for parsing directly to a value or an error report, via a custom unbox-sum-based parser type.
* Added `parseFieldWithDefault`.
* Added `serializeJust`.
* Changed the methods of `JSONObjectSerializer` and `JSONTupleSerializer` to be more consistent with the rest of the library.
* Got rid of mandatory identifiers for object parsers.
* Got rid of mandatory identifiers for object serializers.
* Improved Generic Deriving Mechanisms.
* Lots of optimizations.

## 0.1.0.0 -- ???

* First version. Released on an unsuspecting world.
* I forgot to document this originally.
