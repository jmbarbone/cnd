# Conditions

`condition()` is used to create a new condition function that itself
returns a new `condition`.

`conditions()` retrieves all conditions based on search values. The
parameters serve as filtering arguments.

## Usage

``` r
condition(
  name,
  message = NULL,
  type = c("condition", "message", "warning", "error"),
  package = get_package(),
  exports = NULL,
  help = NULL,
  registry = package,
  register = !is.null(registry),
  classes = NULL,
  class
)

conditions(
  ...,
  class = NULL,
  type = NULL,
  package = NULL,
  registry = NULL,
  fun = NULL
)

cond(x)

cnd(condition)

conditions(x, ...) <- value

# S3 method for class '`function`'
conditions(x, append = FALSE, ...) <- value

# S3 method for class '`cnd::condition_progenitor`'
conditions(x, ...) <- value
```

## Arguments

- name:

  The name of the new class

- message:

  The message to be displayed when the condition is called. When entered
  as a character vector, the message is collapsed into a single string.
  Use explicit line returns to generate new lines in output messages.
  When a function is used and a character vector returned, each element
  is treated as a new line.

- type:

  The type of condition: error, warning, or message

- package:

  The package to which the condition belongs

- exports:

  The exported functions to be displayed when the condition is called

- help:

  The help message to be displayed for the condition function

- registry:

  The name of the registry to store the condition

- register:

  Controls registration checks

- classes:

  Additional classes to add to the condition

- class:

  ***Deprecated***; use `name` instead

- ...:

  Additional arguments passed to methods

- fun:

  if a `function` is passed, then retrieves the `"conditions"` attribute

- x:

  An object

- condition:

  A condition_generator object

- value:

  A `condition`

- append:

  If `TRUE`, adds to the **conditions** attribute

## Value

- `condition()` a condition_generator object

&nbsp;

- `conditions()` a `list` of condition_generator objects

&nbsp;

- `cond()` A condition_generator object

&nbsp;

- `cnd()` is a wrapper for calling
  [`base::stop()`](https://rdrr.io/r/base/stop.html),
  [`base::warning()`](https://rdrr.io/r/base/warning.html), or
  [`base::message()`](https://rdrr.io/r/base/message.html); when
  `condition` is a type, an error is thrown, and likewise for the other
  types. When an error isn't thrown, the `condition` is returned,
  invisibly.

## Details

Conditions

## condition_generator

A condition_generator is an object (a special
[function](https://rdrr.io/r/base/function.html)) which can be used to
create generate a new condition, based on specifications applied in
`condition()`. These functions use `...` to absorb extra arguments and
contain a special `.call` parameter. By default, `.call` captures the
parent call from where the condition_generator was created, but users
may pass their own call to override this. See `call.` in
[`base::conditionCall()`](https://rdrr.io/r/base/conditions.html)

## `condition()` conditions

Conditions are generated through the
[`{cnd}`](https://jmbarbone.github.io/cnd/reference/cnd-package.md)
package. The following conditions are associated with this function:

- [`cnd:condition_as_character_error/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  You cannot coerce a condition_generator object to a character. This
  may have occurred when trying to put a condition function through
  [`base::stop()`](https://rdrr.io/r/base/stop.html) or
  [`base::warning()`](https://rdrr.io/r/base/warning.html). Instead,
  call the function first, then pass the result to
  [`base::stop()`](https://rdrr.io/r/base/stop.html) or
  [`base::warning()`](https://rdrr.io/r/base/warning.html).

  For example:

      # Instead of this
      stop(my_condition)

      # Do this
      stop(my_condition())

- [`cnd:condition_message_error/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  Conditions messages are displayed when invoked through
  [`base::conditionMessage()`](https://rdrr.io/r/base/conditions.html).
  You can set a static message by passing through a `character` vector,
  or a dynamic message by passing through a `function`. The function
  should return a `character` vector.

  When `message` is not set, a default "there was an error" message is
  used.

- [`cnd:condition_message_generator_error/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  condition_generator objects are not conditions. You may have made this
  mistake:

      x <- condition("my_condition")
      conditionMessage(x)

  Condition generators need to be called first before they can be used
  as conditions. Try this instead:

      x <- condition("my_condition")
      conditionMessage(x())

- [`cnd:condition_overwrite_warning/warning`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  Defining a new condition with the same class and package as an
  existing condition will overwrite the previous definition. It is
  recommended to either avoid this by fully defining your condition, or
  creating a new condition instead.

- [`cnd:invalid_condition_error/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  The `class`, `exports`, and `help` parameters must be a single
  character string. If you are passing a function, it must be a valid
  function.

- [`cnd:match_arg/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  Mostly [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html)
  but with a custom condition

- [`cnd:no_package_exports_warning/warning`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  The `exports` parameter requires a `package`

For more conditions, see:
[cnd-cnd-conditions](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md)

## `cnd()` conditions

Conditions are generated through the
[`{cnd}`](https://jmbarbone.github.io/cnd/reference/cnd-package.md)
package. The following conditions are associated with this function:

- [`cnd:cnd_class_error/error`](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md):

  `cnd()` simple calls the appropriate function:
  [`base::stop()`](https://rdrr.io/r/base/stop.html),
  [`base::warning()`](https://rdrr.io/r/base/warning.html), or
  [`base::message()`](https://rdrr.io/r/base/message.html) based on the
  `type` parameter from `condition()`.

For more conditions, see:
[cnd-cnd-conditions](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md)

## See also

[cnd-package](https://jmbarbone.github.io/cnd/reference/cnd-package.md)

## Examples

``` r
# create a new condition:
cond_bad_value <- condition("bad_value", type = "error")

# use the condition
try(stop(cond_bad_value()))
#> Error in eval() : <bad_value>
#> there was an error
try(cnd(cond_bad_value()))
#> Error in eval() : <bad_value>
#> there was an error

# dynamic messages:
cond_class_error <- condition(
  "class_error",
  message = function(x) paste("class cannot be", toString(class(x))),
  type = "error"
)
try(stop(cond_class_error(list())))
#> Error in eval() : <class_error>
#> class cannot be list
```
