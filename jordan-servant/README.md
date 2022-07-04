# `jordan-servant`: Combinators for Jordan Types

This module provides interopability between Jordan and Servant.
More specificically, it provides:

- A query-string-parser interface for `Jordan.FromJSON.Class.FromJSON`, which lets you take any type you can parse from JSON, and parse it from a *query string* at a given key.
- A query-string-serializer interface for `Jordan.ToJSON.Class.ToJSON`, which lets you take any type you can serialize to JSON, and serialize it out to a *query string* at a given key.
- Servant combinator types for working with query strings that are pased via Jordan.
- An API Combinator called `ReportingRequestBody`, which will try to parse a request body using `Jordan`.

This package is best used with its sister packages, `jordan-servant-server` and `jordan-servant-client`.
