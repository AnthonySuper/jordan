# Jordan: Abstract, Inspectable JSON for Haskell

Jordan provides an abstract, inspectable way to work with JSON representations of Haskell objects.
It uses a variant of *finally-tagless style* in order to represent JSON serialization and parsing as *instructions*, which can then be used to make *parsers* or *serializers*.
Notably, this means that Jordan *avoids intermediate maps* and other data structures: Jordan serialization writes directly to Text, and Jordan deserialization parses *directly to objects*!

This repository consists of three projects:

- [*jordan*](./jordan/README.md), located in `jordan/`, provides core functionality.
  More specifically, it provides:
    - An abstract API for representing JSON serialization
    - An abstract API for representing JSON parsing
    - A concrete, megaparsec-based JSON parser
    - A concrete, attoparsec-based JSON parser
    - A concrete, text-writer-based JSON serializer
    - A concrete, Builder-based JSON serializer
- *jordan-openapi*, which provides a way to generate OpenAPI documentation from the abstract representation of serialization or deserialization Jordan provides.
  This is a pretty basic package, but demonstrates how inspectable these parsers are!
- *jordan-servant*, which lets you use your jordan serializers and deserializers with servant.
  This is an extremely basic package.

This project is *highly alpha* and should be considered a proof of concept.
