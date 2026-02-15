# Default conditions

Default conditions have options for dynamically generated messages based
on the parameters provided.

## Usage

``` r
value_error(..., .call = getOption("cnd.call", TRUE))

class_error(
  ...,
  x,
  actual = x,
  actual_class = class(actual),
  expected,
  expected_class = class(expected),
  name,
  .call = getOption("cnd.call", TRUE)
)

type_error(
  ...,
  x,
  actual = x,
  actual_type = typeof(actual),
  expected,
  expected_type = typeof(expected),
  name,
  .call = getOption("cnd.call", TRUE)
)

input_error(..., .call = getOption("cnd.call", TRUE))

use_error(..., .call = getOption("cnd.call", TRUE))

defunct_error(..., defunct, replacement, .call = getOption("cnd.call", TRUE))

deprecated_warning(
  ...,
  deprecated,
  replacement,
  version,
  .call = getOption("cnd.call", TRUE)
)

value_warning(..., .call = getOption("cnd.call", TRUE))

type_warning(
  ...,
  x,
  actual = x,
  actual_type = typeof(actual),
  expected,
  expected_type = typeof(expected),
  name,
  .call = getOption("cnd.call", TRUE)
)

input_warning(..., .call = getOption("cnd.call", TRUE))

class_warning(
  ...,
  x,
  actual = x,
  actual_class = class(actual),
  expected,
  expected_class = class(expected),
  name,
  .call = getOption("cnd.call", TRUE)
)

use_warning(..., .call = getOption("cnd.call", TRUE))
```

## Arguments

- ...:

  Additional message components; extra named arguments will signal an
  `input_warning()`.

- x:

  an object

- actual, expected:

  Objects whose `class` or `type` should be retrieved

- actual_class, actual_type, expected_class, expected_type:

  Override `class` or `type`

- name:

  Name of the object (will be deparsed if not provided)

- defunct, deprecated, replacement:

  Defunct, deprecated and replacement object, use
  [`base::quote()`](https://rdrr.io/r/base/substitute.html) to pass
  expressions (e.g., `quote(fun(old = ))`)

- version:

  A version number

## Value

A condition object

## Details

If no values are entered into the
[condition_generator](https://jmbarbone.github.io/cnd/reference/condition.md),
a default message will be used. Messages will be dynamically created
based on the parameters provided.

## messages

Messages are dynamically created with specific inputs per condition.
Generally, parameters must be explicitly used. All condition messages
use `...` which allow for either additional context to be added or
completely overriding the message. Default messages will require the
first parameter to be used.

## Examples

``` r
type_error()
#> type_error/error
#> (type_error/cnd::condition/error/condition)
#> Invalid type
type_error(x = 1L)
#> type_error/error
#> (type_error/cnd::condition/error/condition)
#> Invalid type for `1L`: got 'integer'
type_error(x = 1L, expected = double())
#> type_error/error
#> (type_error/cnd::condition/error/condition)
#> Invalid type for `1L`: got 'integer', expected 'double'
type_error(x = 1, actual = integer(), expected = double())
#> type_error/error
#> (type_error/cnd::condition/error/condition)
#> Invalid type for `1`: got 'integer', expected 'double'
type_error(
  "Additional context",
  x = "a",
  expected = 1L
)
#> type_error/error
#> (type_error/cnd::condition/error/condition)
#> Invalid type for `"a"`: got 'character', expected 'integer'
#> Additional context
```
