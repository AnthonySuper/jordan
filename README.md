# Jordan: Abstract, Inspectable JSON for Haskell

Jordan provides an abstract, inspectable way to work with JSON representations of Haskell objects.
It uses a variant of *finally-tagless style* in order to represent JSON serialization and parsing as *instructions*, which can then be used to make *parsers* or *serializers*.
Notably, this means that Jordan *avoids intermediate maps* and other data structures: Jordan serialization writes directly to Text, and Jordan deserialization parses *directly to objects*!

This repository consists of several, small haskell packages:

- [*jordan*](./jordan/README.md), located in `jordan/`, provides core functionality.
  More specifically, it provides:
    - An abstract API for representing JSON serialization
    - An abstract API for representing JSON parsing
    - A concrete, attoparsec-based JSON parser
    - A concrete, custom-parsing based "reporting parser", which generates nice error reports for JSON that is syntactically valid but semantically wrong for the type
    - A concrete, Builder-based JSON serializer
- [*jordan-openapi*](./jordan-openapi/README.md), which provides a way to generate OpenAPI documentation from the abstract representation of serialization or deserialization Jordan provides.
- [*jordan-servant*](./jordan-servant/README.md), which provides API combinators for use with Servant, as well as:
    - A Jordan parser which parses from a *query string*, letting you use any instance of `Jordan.FromJSON.Class.FromJSON` to parse parts of a query string.
    - A Jordan serializer which serializes to a *query string*, letting you use any instance of `Jordan.ToJSON.Class.ToJSON` to build part of a query string
- [*jordan-servant-server*](./jordan-servant-server/README.md), which provides server-side functionality for Jordan servant combinators.
  Uniquely, this package provides an *error report* on invalid JSON, informing the user why their JSON was not accepted by the server in a standardized way.
- [*jordan-servant-client*](./jordan-servant-client/README.md), which provides client-side functionality for Jordan servant combinators.
  This allows you to talk to a Jordan-enhanced API from Haskell code.
- [*jordan-servant-openapi*](./jordan-servant-openapi/README.md), which provides automatic documentation for APIs built with `jordan-servant`.
  Since this documentation is generated from the `FromJSON` instances you defined with `jordan`, it can *never* get out of sync with how your actual API works!

This project is *highly alpha* and should be considered a proof of concept.

## What makes Jordan different

Jordan, like the venerable [aeson](https://hackage.haskell.org/package/aeson) package, provides a way to turn your Haskell types to and from JSON.
Unlike Aeson, however, Jordan has an intentionally *restricted* interface, which forces you to describe your parsers and serializers as a sequence of *instructions*.
So, instead of defining a function that actually keys into some representation of a JSON object, you define things abstractly:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import GHC.Generics
import Jordan
import Jordan.Types.JSONError

data Person = Person { firstName :: Text, lastName :: Text }
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Person where
  fromJSON = parseObject $ Person <$> parseField "firstName" <*> parseField "lastName"

instance ToJSON Person where
  toJSON = serializeObject $ divide dividePerson (serializeField "firstName") (serializeField "lastName")
    where
      dividePerson (Person first last) = (first, last)

myPerson :: Person
myPerson = Person "Richard" "Evans"
```

This abstract representation allows us to "plug in" different parsers.
For example, we have basic JSON parsing:

```haskell

parsedAtto :: Either String Person
parsedAtto = parseViaAttoparsec "{ \"firstName\": \"Mike\", \"lastName\": \"Stoklassa\" }"
```

But, since the way we parse is *abstract*, we can also swap out *other parsers*.
For example, here's one that uses a special "error reporting" parser, which will give you back a nice description of "what went wrong" if supplied invalid JSON:

```haskell
-- A special parser that will give us an error report if something is wrong
parsedReport :: Either JSONError Person
parsedReport = parseOrReport "{ \"firstName\": \"Jack\", \"lastName\": [] }"
```

We can also serialize JSON, of course:

```haskell
viaBuilder = toJSONViaBuilder myPerson
```

And, since this is a literate Haskell file, I do need a main.
Here's that:

```haskell
main = do
  print parsedAtto
  print parsedReport
  print viaBuilder
```
