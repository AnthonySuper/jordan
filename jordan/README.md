# Jordan: Abstract, Inspectable JSON for Haskell

Jordan provides an abstract interface to work with JSON representations of Haskell objects.
It uses a variant of a *finally-tagless style* in order to represent the act of parsing or serializing JSON as a series of *abstract instructions*, which can then be converted to *parsers*, *serializers*, or *documentation generators*.

This provides a few benefits:

- It's easy to plug in different ways to parse or serialize JSON, as everything is defined in terms of *instructions* instead of a concrete implementation.
- Documentation can be generated automatically, *from those instructions*, so it will always be correct.
- Efficient parsers that do not construct any sort of intermediate representations (like a `Map Text JSONValue` or whatever) are possible to construct (and are used in this package).

## Example

Let's say I have a Haskell datatype, which looks like this:

```haskell
data Person
  = Person
  { name :: Text
  , age :: Int 
  }
```

I want to be able to convert this type to and from JSON.
With Jordan, I can implement the `FromJSON` and `ToJSON` typeclasses:

```haskell
instance FromJSON Person where
  fromJSON = parseObject "Person.Input"
    (Person <$> parseField "name" <*> parseField "age")

instance ToJSON Person where
  toJSON = serializeObject "Person.Output" $
    divide
      (\(Person name age) -> (name, age))
      (writeField "name" serializeText)
      (writeField "age" $ contramap realToFrac serializeNumber)
```

These typeclasses provide *instructions* for how to parse and serialize JSON representations of my `Person` type.
In this case, I specified that you parse my object by reading some fields from an object, and serialize it by writing some fields into an object.
But, I've kept the instructions *abstract*.

## Types

More specifically, `FromJSON` has the following signature:

```haskell
class FromJSON value where
  fromJSON :: (JSONParser f) => f value
```

And `ToJSON` is defined as:

```haskell
class ToJSON v where
  toJSON :: (JSONSerializer f) => f v
```

Note that the type of the *particular* parser or serializer is kept *abstract*.
This means that we can plug in different implementations.
By default, Jordan comes with the following:

- A `megaparsec`-based parser that parses from text
- An `attoparsec`-based parser that parses from a UTF-8 ByteString
- A "text writer"-based serializer that serilaizes to text
- A `Data.ByteString.Builder`-based serializer that serializes to a UTF-8 encoded JSON string
