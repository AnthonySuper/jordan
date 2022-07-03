# `jordan-servant-server`: Server combinators for `jordan-servant`.

This package provides `HasServer` functionality for Jordan servant combinators.
This allows you to use Jordan types in your servant APIs, making them easier to program.

The server functionality provided for the `ReportingRequestBody` combinator in the base `jordan-servant` package will automatically provide an *error report* for any syntactically invalid JSON.
That is, if somebody tries to `POST` or `PUT` a value that is valid JSON syntactically, but not valid JSON *semantically* for your API (IE, it's missing keys you need or something), it will respond with a `400 BAD REQUEST` and an error reporting body.
This body has a special content type so you can provide your own `BAD REQUEST` combinators if you wish.
