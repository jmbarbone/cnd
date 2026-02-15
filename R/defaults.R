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
#' @param ... Additional message components; extra named arguments will signal
#'   an [cnd::input_warning()].
#' @param x an object
#' @param actual,expected Objects whose `class` or `type` should be retrieved
#' @param actual_class,actual_type,expected_class,expected_type Override `class`
#'   or `type`
#' @param name Name of the object (will be deparsed if not provided)
#' @param defunct,deprecated,replacement Defunct, deprecated and replacement
#'   object, use [base::quote()] to pass expressions (e.g., `quote(fun(old =
#'   ))`)
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
#'   expected = 1L
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
    function(...) {
      check_dots(...)
      .msg(...) %||% "Invalid value"
    },
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
    function(
      ...,
      x,
      actual = x,
      actual_class = class(actual),
      expected,
      expected_class = class(expected),
      name
    ) {
      check_dots(...)

      if (missing(x)) {
        return(.msg(...) %||% "Invalid class")
      }

      if (missing(name)) {
        mc <- match.call(
          sys.function(sys.parent(1L)),
          sys.call(sys.parent(1L)),
          envir = parent.frame(3L)
        )
        name <- sprintf("`%s`", deparse(mc$x %||% mc$actual))
      }

      msg <- sprintf(
        paste0(
          "Invalid class ",
          paste("for", name, recycle0 = TRUE),
          ": got '%s'"
        ),
        strings(actual_class)
      )

      if (!missing(expected) || !missing(expected_class)) {
        msg <- sprintf("%s, expected '%s'", msg, strings(expected_class))
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
    function(
      ...,
      x,
      actual = x,
      actual_type = typeof(actual),
      expected,
      expected_type = typeof(expected),
      name
    ) {
      check_dots(...)

      if (missing(x)) {
        return(.msg(...) %||% "Invalid type")
      }

      force(x)

      if (missing(name)) {
        mc <- match.call(
          sys.function(sys.parent(1L)),
          sys.call(sys.parent(1L)),
          envir = parent.frame(3L)
        )
        name <- sprintf("`%s`", deparse(mc$x %||% mc$actual))
      }

      msg <- sprintf(
        paste0(
          "Invalid type ",
          paste("for", name, recycle0 = TRUE),
          ": got '%s'"
        ),
        actual_type
      )

      if (!missing(expected) || !missing(expected_type)) {
        msg <- sprintf("%s, expected '%s'", msg, expected_type)
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
    function(...) {
      check_dots(...)
      .msg(...) %||% "Invalid input"
    },
    type = "error",
    package = NULL,
    help = "Generic input error"
  )
)

#' @export
#' @rdname defaults
use_error <- function() {}
delayedAssign(
  "use_error",
  condition(
    "use_error",
    function(...) {
      check_dots(...)
      .msg(...) %||% "Invalid use of function or objects"
    },
    type = "error",
    package = NULL,
    help = "Generic use error"
  )
)

#' @export
#' @rdname defaults
defunct_error <- function() {}
delayedAssign(
  "defunct_error",
  condition(
    "defunct_error",
    function(..., defunct, replacement) {
      check_dots(...)

      if (missing(defunct)) {
        return(.msg(...) %||% "Defunct feature")
      }

      msg <- sprintf("%s is defunct", ticks(defunct))

      if (!missing(replacement)) {
        msg <- sprintf("%s, use %s instead", msg, ticks(replacement))
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
      check_dots(...)

      if (missing(deprecated)) {
        return(.msg(...) %||% "Deprecated feature")
      }

      msg <- sprintf("%s is deprecated", ticks(deprecated))

      if (!missing(replacement)) {
        msg <- sprintf("%s, use %s instead", msg, ticks(replacement))
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

#' @export
#' @rdname defaults
use_warning <- function() {}
delayedAssign("use_warning", convert(use_error, "warning"))

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

ticks <- function(x) {
  if (is.call(x)) {
    x <- sprintf("`%s`", format(x))
  }
  x
}

check_dots <- function(...) {
  dots <- match.call(expand.dots = FALSE)$...

  if (is.null(dots)) {
    return()
  }

  bad <- names(dots)
  bad <- bad[nzchar(bad)]

  if (length(bad)) {
    cnd(
      input_warning(
        "these parameters are being absorbed into the condition message: ",
        strings(bad)
      )
    )
  }
}

# nocov start
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
# nocov end
