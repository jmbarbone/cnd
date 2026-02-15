# TODO consider using `msg = NULL` for users to overwrite the message; or append
# additional information

#' Default conditions
#'
#' Default conditions have options for dynamically generated messages based on
#' the parameters provided.
#'
#' @section messages: Messages are dynamically created with specific inputs per
#'   condition. Generally, parameters must be explicitly used.  All condition
#'   messages use `...` which allow for either additional context to be added or
#'   completely overriding the message.  Default messages will require the first
#'   parameter to be used.
#'
#' @param ... Additional message components
#' @param x an object
#' @param actual,expected Actual or expected value (see details)
#' @param name Name of the object (will be deparsed if not provided)
#' @param defunct,deprecated,replacement Defunct, deprecated and replacement
#'   object, use [base::quote()] to pass expressions (e.g., `quote(old_fun()`)
#' @param version A version number
#' @details If no values are entered into the [cnd::condition_generator], a
#'   default message will be used. Messages will be dynamically created based on
#'   the parameters provided.
#'
#' @examples
#' type_error()
#' type_error(x = 1L)
#' type_error(x = 1L, expected = double())
#' type_error(x = 1, actual = integer(), expected = double())
#' type_error(
#'   "Additional context",
#'   x = "a",
#'   actual = I("string"),
#'   expected = I("numeric or integer")
#' )
#'
#' @return A condition object
#' @keywords internal
#' @name defaults
NULL

# errors ------------------------------------------------------------------

#' @export
#' @rdname defaults
value_error <- function() {}
delayedAssign(
  "value_error",
  condition(
    "value_error",
    \(...) .msg(...) %||% "Invalid value",
    type = "error",
    package = NULL,
    help = "Generic value error"
  )
)

#' @export
#' @rdname defaults
class_error <- function() {}
delayedAssign(
  "class_error",
  condition(
    "class_error",
    function(..., x, expected, actual = x, name) {
      if (missing(x)) {
        return(.msg(...) %||% "Invalid class")
      }

      force(x)

      if (missing(name)) {
        name <- sprintf(
          "`%s`",
          deparse(
            match.call(
              sys.function(sys.parent(1L)),
              sys.call(sys.parent(1L)),
              envir = parent.frame(3L)
            )$x
          )
        )
      }

      if (inherits(actual, "AsIs")) {
        actual <- actual
      } else {
        actual <- class(actual)
      }
      force(actual)

      msg <- sprintf(
        paste0(
          "Invalid class ",
          paste("for", name, recycle0 = TRUE),
          ": got '%s'"
        ),
        strings(actual)
      )

      if (!missing(expected)) {
        if (inherits(expected, "AsIs")) {
          expected <- expected
        } else {
          expected <- class(expected)
        }
        # option for ::?
        msg <- sprintf("%s, expected '%s'", msg, strings(expected))
      }

      if (...length()) {
        msg <- sprintf("%s\n%s", msg, .msg(...))
      }

      msg
    },
    type = "error",
    package = NULL,
    help = "Generic class error"
  )
)

#' @export
#' @rdname defaults
type_error <- function() {}
delayedAssign(
  "type_error",
  condition(
    "type_error",
    function(..., x, expected, actual = x, name) {
      if (missing(x)) {
        return(.msg(...) %||% "Invalid type")
      }

      force(x)

      if (missing(name)) {
        name <- sprintf(
          "`%s`",
          deparse(
            match.call(
              sys.function(sys.parent(1L)),
              sys.call(sys.parent(1L)),
              envir = parent.frame(3L)
            )$x
          )
        )
      }

      if (inherits(actual, "AsIs")) {
        actual <- actual
      } else {
        actual <- typeof(actual)
      }
      force(actual)

      msg <- sprintf(
        paste0(
          "Invalid type ",
          paste("for", name, recycle0 = TRUE),
          ": got '%s'"
        ),
        actual
      )

      if (!missing(expected)) {
        if (inherits(expected, "AsIs")) {
          expected <- expected
        } else {
          expected <- typeof(expected)
        }
        # option for ::?
        msg <- sprintf("%s, expected '%s'", msg, expected)
      }

      if (...length()) {
        msg <- sprintf("%s\n%s", msg, .msg(...))
      }

      msg
    },
    type = "error",
    package = NULL,
    help = "Generic type error"
  )
)

#' @export
#' @rdname defaults
input_error <- function() {}
delayedAssign(
  "input_error",
  condition(
    "input_error",
    function(...) .msg(...) %||% "Invalid input",
    type = "error",
    package = NULL,
    help = "Generic input error"
  )
)

#' @export
#' @rdname defaults
defunct_error <- function() {}
delayedAssign(
  "defunct_error",
  condition(
    "defunct_error",
    function(..., defunct, replacement, version) {
      if (missing(defunct)) {
        return(.msg(...) %||% "Defunct feature")
      }

      msg <- sprintf("%s is defunct", deparse1(defunct))
      if (!missing(replacement)) {
        msg <- sprintf("%s, use %s instead", msg, deparse1(replacement))
      }

      if (missing(version) || isTRUE(version)) {
        msg <- paste(msg, "and has been removed")
      } else if (!isFALSE(version)) {
        msg <- sprintf("%s and has been removed in %s", msg, version)
      }

      msg
    },
    type = "error",
    package = NULL,
    help = "Generic defunct error"
  )
)

# warnings ----------------------------------------------------------------

#' @export
#' @rdname defaults
deprecated_warning <- function() {}
delayedAssign(
  "deprecated_warning",
  condition(
    "deprecated_warning",
    function(..., deprecated, replacement, version) {
      if (missing(deprecated)) {
        return(.msg(...) %||% "Deprecated feature")
      }

      msg <- sprintf("%s is deprecated", deparse1(deprecated))

      if (!missing(replacement)) {
        msg <- sprintf("%s, use %s instead", msg, deparse1(replacement))
      }

      if (missing(version) || isTRUE(version)) {
        msg <- paste(msg, "and will be removed in a future version")
      } else if (!isFALSE(version)) {
        msg <- sprintf("%s and will be removed in %s", msg, version)
      }

      msg
    },
    type = "warning",
    package = NULL,
    help = "Generic deprecation warning"
  )
)

#' @export
#' @rdname defaults
value_warning <- function() {}
delayedAssign("value_warning", convert(value_error, "warning"))

#' @export
#' @rdname defaults
type_warning <- function() {}
delayedAssign("type_warning", convert(type_error, "warning"))

#' @export
#' @rdname defaults
input_warning <- function() {}
delayedAssign("input_warning", convert(input_error, "warning"))

#' @export
#' @rdname defaults
class_warning <- function() {}
delayedAssign("class_warning", convert(class_error, "warning"))


# helpers -----------------------------------------------------------------

.msg <- function(...) {
  if (...length()) {
    dots <- list(...)
    dots$collapse <- ""
    do.call(paste0, dots)
  }
}

strings <- function(...) {
  paste0(c(...), collapse = ", ")
}

convert <- function(cnd, new) {
  change <- function(x) sub(cnd$type, new, x, fixed = TRUE)
  condition(
    name = change(cnd$class),
    message = cnd$message,
    type = new,
    package = cnd$package,
    help = change(cnd$help)
  )
}
