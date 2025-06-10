# TODO consider using `msg = NULL` for users to overwrite the message; or append
# additional information

InputError <- function() {}
delayedAssign(
  "InputError",
  condition(
    "input_error",
    message = function(x) {
      fmt("input is not valid:\n{x}", x = format(x))
    },
    type = "error"
  )
)

InputWarning <- function() {}
delayedAssign(
  "InputWarning",
  condition(
    "input_warning",
    # TODO something other than "valid"?
    message = function(x) {
      fmt("input is not valid:\n{x}", x = format(x))
    },
    type = "warning"
  )
)

ValueError <- function() {}
delayedAssign(
  "ValueError",
  condition(
    "value_error",
    message = function(x) {
      fmt("value is not valid:\n{x}", x = format(x))
    },
    type = "error"
  )
)

ValueWarning <- function() {}
delayedAssign(
  "ValueWarning",
  condition(
    "value_warning",
    # TODO something other than "valid"?
    message = function(x) {
      fmt("value is not valid:\n{x}", x = format(x))
    },
    type = "warning"
  )
)

MatchError <- function() {}
delayedAssign(
  "MatchError",
  condition(
    "match_error",
    message = function(x, choices) {
      fmt(
        "match failed\n  input:   {x}\n  choices: {choices}",
        x = x,
        choices = collapse(choices, sep = ", ")
      )
    },
    type = "error"
  )
)

PathOverwriteMessage <- function() {}
delayedAssign(
  "PathOverwriteMessage",
  condition(
    "path_overwrite_message",
    message = function(path) {
      fmt(
        "Overwriting path: {path}",
        path = normalizePath(path, mustWork = FALSE)
      )
    },
    type = "message"
  )
)

PathDeletionMessage <- function() {}
delayedAssign(
  "PathDeletionMessage",
  condition(
    "path_deletion_message",
    message = function(path) {
      fmt(
        "Deleting path: {path}",
        path = normalizePath(path, mustWork = FALSE)
      )
    },
    type = "message"
  )
)

# fmt: skip
DeprecationWarning <- function() {}
delayedAssign(
  "DeprecationWarning",
  condition(
    "deprecation_warning",
    message = function(x, replacement) {
      fmt(
        "{x} is deprecated and will be removed in a future version\n",
        "use {replacement} instead",
        x = x,
        replacement = replacement
      )
    },
    type = "warning"
  )
)

DefunctError <- function() {}
delayedAssign(
  "DefunctError",
  condition(
    "defunct_error",
    message = function(x) {
      fmt("{x} is defunct", x = x)
    },
    type = "error"
  )
)

TypeError <- function() {}
delayedAssign(
  "TypeError",
  condition(
    "type_error",
    # TODO incorporate test #11
    # test = function(x, type = typeof(example), example = NULL) {
    #   force(type)
    #   identical(typeof(x), type)
    # },
    message = function(x, type) {
      fmt("'{x}' is not of type {type}", x = x, type = type)
    },
    type = "error"
  )
)

ClassError <- function() {}
delayedAssign(
  "ClassError",
  condition(
    "class_error",
    # TODO incorporate test #11
    # test = function(x, class = class(example), example = NULL) {
    #   force(class)
    #   inherits(x, class)
    # },
    message = function(x, class) {
      fmt("'{x}' is not of class {class}", x = x, class = collapse(class, ","))
    },
    type = "error"
  )
)

NamespaceError <- function() {}
