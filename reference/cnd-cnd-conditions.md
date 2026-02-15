# Conditions for `cnd`

Conditions for `cnd`

## Details

The following conditions are defined in the `{cnd}` package.

## `{cnd}`

These conditions are made with the
[`{cnd}`](https://jmbarbone.github.io/cnd/reference/cnd-package.md)
package though the use of
[`condition()`](https://jmbarbone.github.io/cnd/reference/condition.md).

## `{cnd}` conditions

### `cnd:cnd_class_error/error`

- package:

  `{cnd}`

- class:

  `cnd:cnd_class_error`

- type:

  **error**

[`cnd()`](https://jmbarbone.github.io/cnd/reference/condition.md) simple
calls the appropriate function:
[`base::stop()`](https://rdrr.io/r/base/stop.html),
[`base::warning()`](https://rdrr.io/r/base/warning.html), or
[`base::message()`](https://rdrr.io/r/base/message.html) based on the
`type` parameter from
[`condition()`](https://jmbarbone.github.io/cnd/reference/condition.md).

### `cnd:cnd_document_conditions/warning`

- package:

  `{cnd}`

- class:

  `cnd:cnd_document_conditions`

- type:

  **warning**

Documentation will fail when no conditions are found. You may be
executing
[`cnd_document()`](https://jmbarbone.github.io/cnd/reference/cnd_document.md)
too early, before conditions have been registered. You can try to find
your conditions with
[`conditions()`](https://jmbarbone.github.io/cnd/reference/condition.md).

### `cnd:cnd_document_file/error`

- package:

  `{cnd}`

- class:

  `cnd:cnd_document_file`

- type:

  **error**

The `file` argument to
[`cnd_document()`](https://jmbarbone.github.io/cnd/reference/cnd_document.md)
must be a file path, a connection object, or `NULL` to return the
documentation as a character vector. The default value should be
suitable for standard use cases.

### `cnd:cnd_document_pkg_reg/error`

- package:

  `{cnd}`

- class:

  `cnd:cnd_document_pkg_reg`

- type:

  **error**

Both `package` and `registry` must be set to document conditions.You can
set a registry by adding
[`cnd_create_registry()`](https://jmbarbone.github.io/cnd/reference/cnd_create_registry.md)
calls to your package code.

### `cnd:cnd_generated_cleanup/message`

- package:

  `{cnd}`

- class:

  `cnd:cnd_generated_cleanup`

- type:

  **message**

Some files created during the documentation process may become obsolete
while updating your conditions.

### `cnd:cnd_generated_write/condition`

- package:

  `{cnd}`

- class:

  `cnd:cnd_generated_write`

- type:

  **condition**

This condition is signaled when
[`cnd_document()`](https://jmbarbone.github.io/cnd/reference/cnd_document.md)
needs to write new documentation files.

### `cnd:condition_as_character_error/error`

- package:

  `{cnd}`

- class:

  `cnd:condition_as_character_error`

- type:

  **error**

You cannot coerce a
[condition_generator](https://jmbarbone.github.io/cnd/reference/condition.md)
object to a character. This may have occurred when trying to put a
condition function through
[`base::stop()`](https://rdrr.io/r/base/stop.html) or
[`base::warning()`](https://rdrr.io/r/base/warning.html). Instead, call
the function first, then pass the result to
[`base::stop()`](https://rdrr.io/r/base/stop.html) or
[`base::warning()`](https://rdrr.io/r/base/warning.html).

For example:

    # Instead of this
    stop(my_condition)

    # Do this
    stop(my_condition())

### `cnd:condition_message_error/error`

- package:

  `{cnd}`

- class:

  `cnd:condition_message_error`

- type:

  **error**

Conditions messages are displayed when invoked through
[`base::conditionMessage()`](https://rdrr.io/r/base/conditions.html).
You can set a static message by passing through a `character` vector, or
a dynamic message by passing through a `function`. The function should
return a `character` vector.

When `message` is not set, a default "there was an error" message is
used.

### `cnd:condition_message_generator_error/error`

- package:

  `{cnd}`

- class:

  `cnd:condition_message_generator_error`

- type:

  **error**

[condition_generator](https://jmbarbone.github.io/cnd/reference/condition.md)
objects are not conditions. You may have made this mistake:

    x <- condition("my_condition")
    conditionMessage(x)

Condition generators need to be called first before they can be used as
conditions. Try this instead:

    x <- condition("my_condition")
    conditionMessage(x())

### `cnd:condition_overwrite_warning/warning`

- package:

  `{cnd}`

- class:

  `cnd:condition_overwrite_warning`

- type:

  **warning**

Defining a new condition with the same class and package as an existing
condition will overwrite the previous definition. It is recommended to
either avoid this by fully defining your condition, or creating a new
condition instead.

### `cnd:conditions_dots_warning/warning`

- package:

  `{cnd}`

- class:

  `cnd:conditions_dots_warning`

- type:

  **warning**

The `...` parameter in
[`conditions()`](https://jmbarbone.github.io/cnd/reference/condition.md)
is meant for convenience. Only a single argument is allowed. Other
parameters must be named explicitly.

For example:

    # Instead of this
    conditions("class", "package") # "package" is ignored with a warning

    # Do this
    conditions(class = "class", package = "package")

### `cnd:internal_error/error`

- package:

  `{cnd}`

- class:

  `cnd:internal_error`

- type:

  **error**

This is an internal error, which means that something has gone
(horribly?) wrong within
[cnd](https://jmbarbone.github.io/cnd/reference/condition.md). If you
believe this is a problem please provide a report at
<https://github.com/jmbarbone/cnd/issues>

### `cnd:invalid_condition_error/error`

- package:

  `{cnd}`

- class:

  `cnd:invalid_condition_error`

- type:

  **error**

The `class`, `exports`, and `help` parameters must be a single character
string. If you are passing a function, it must be a valid function.

### `cnd:match_arg/error`

- package:

  `{cnd}`

- class:

  `cnd:match_arg`

- type:

  **error**

Mostly [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html) but
with a custom condition

### `cnd:no_package_exports_warning/warning`

- package:

  `{cnd}`

- class:

  `cnd:no_package_exports_warning`

- type:

  **warning**

The `exports` parameter requires a `package`

## See also

[cnd-package](https://jmbarbone.github.io/cnd/reference/cnd-package.md)
[condition](https://jmbarbone.github.io/cnd/reference/condition.md)
