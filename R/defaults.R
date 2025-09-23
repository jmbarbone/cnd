# TODO consider using `msg = NULL` for users to overwrite the message; or append
# additional information

fmt0 <- function(msg, x) {
  s <- substitute(x, parent.frame(2L))
  paste0(
    msg,
    if (!missing(x)) fmt("\n{x} = {f}", x = deparse1(s), f = format(x))
  )
}

#' Standard conditions
#'
#' A set of preset conditions
#'
#' @param x An object
#' @param ... additional arguments passed to the condition message
#' @param .call condition arguments
#' @returns A condition generator
#' @name standard-conditions
#' @examples
#' assert <- function(object, is = character()) {
#'   nm <- as.character(substitute(object))
#'   cl <- class(is)
#'   if (!inherits(object, cl)) {
#'     cnd(ClassError(nm, cl))
#'   }
#' }
#'
#' foo <- function(x) {
#'   assert(x, integer())
#'   invisible(x)
#' }
#'
#' foo(1L)
#' try(foo(1.0))
#' try(foo("1"))
#'
#' bar <- function(y) {
#'   assert(y, structure(list(), class = c("class1", "class2")))
#'   invisible(y)
#' }
#'
#' try(bar(1L))
NULL


#' @rdname standard-conditions
#' @export
InputError <- function() {}
delayedAssign(
  "InputError",
  condition(
    "input_error",
    message = function(x) fmt0("input is not valid", x),
    type = "error"
  )
)

#' @rdname standard-conditions
#' @export
InputWarning <- function() {}
delayedAssign(
  "InputWarning",
  condition(
    "input_warning",
    # TODO something other than "valid"?
    message = function(x) {
      c(
        "input is not valid",
        if (!missing(x)) fmt("{x}:\n{f}", x = deparse1(x), f = format(x))
      )
    },
    type = "warning"
  )
)

#' @rdname standard-conditions
#' @export
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

#' @rdname standard-conditions
#' @export
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

#' @rdname standard-conditions
#' @param choices A character vector of choices
#' @export
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

#' @rdname standard-conditions
#' @param path A path
#' @export
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

#' @rdname standard-conditions
#' @export
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

#' @rdname standard-conditions
#' @param replacement an object suggesting the replacement
#' @export
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

#' @rdname standard-conditions
#' @export
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

#' @rdname standard-conditions
#' @param type a type as returned by [typeof()]
#' @export
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

#' @rdname standard-conditions
#' @param class a class or vector of classes
#' @export
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
      fmt(
        "'{x}' is not of class '{class}'",
        x = x,
        class = collapse(class, sep = ", ")
      )
    },
    type = "error"
  )
)

#' @rdname standard-conditions
#' @param pkg A package name
#' @export
NamespaceError <- function() {}
delayedAssign(
  "NamespaceError",
  condition(
    "namespace_error",
    message = function(pkg) {
      fmt("package '{pkg}' is not available", pkg = pkg)
    },
    type = "error"
  )
)
