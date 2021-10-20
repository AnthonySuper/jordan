# Jordan: Abstract, Inspectable JSON for Haskell

Jordan provides an abstract, inspectable way to work with JSON representations of Haskell objects.
It uses a variant of *finally-tagless style* in order to represent JSON serialization and parsing as *instructions*, which can then be used to make *parsers* or *serializers*.
Notably, this means that Jordan *avoids intermediate maps* and other data structures: Jordan serialization writes directly to Text, and Jordan deserialization parses *directly to objects*!

This repository consists of two projects:

- *jordan*, located in `jordan/`, provides core functionality.
  More specifically, it provides:
    - An abstract API for representing JSON serialization
    - An abstract API for representing JSON parsing
    - A concrete, megaparsec-based JSON parser
    - A concrete, attoparsec-based JSON parser
    - A concrete, text-writer-based JSON serializer
    - A concrete, Builder-based JSON serializer
- *jordan-openapi*, which provides a way to generate OpenAPI documentation from the abstract representation of serialization or deserialization Jordan provides.
  This is a pretty basic package, but demonstrates how inspectable these parsers are!

This project is *highly alpha* and more of a proof-of-concept than anything.

## Example

Let's say I have a Haskell datatype, which looks like this:

```haskell
data Person
  = Person
  { name :: Text
  , age :: Int 
  }
```

With Jordan, I can write *instructions* on how to convert this to and from JSON. Like so:

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

Note that everything here is kept abstract: by using the [Applicative](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative) typeclass for parsing, and the [divisible](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative) typeclass for serialization, we define *abstract instructions* for how to convert our type to or from JSON.
This means that we can then *generate a parser* that specifically works on our type, *directly serialize* without any intermediate structures, and *derive documentation* from our instructions.
Even better, this is all doable *generically*: for most datatypes, the compiler can generate the instructions for you, as long as you have an instance of `Generic` available!

## Benefits

- JSON parsing/serialization is kept *entirely abstract*, so you can plug in new parsers or serializers easily.
- The lack of intermediate data structures avoids the need for things like `toEncoding` from aeson: it can be provided directly
- You get to say "Yes, I use divisible contravariant functors in production," which has to feel good.
  Edward Kmett might even let you into category theory nirvana after you die.

## Performance

I honestly have no idea, unfortunately.
I am trying to work on some benchmarks now.
