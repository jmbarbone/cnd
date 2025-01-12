
#' @export
`print.cnd::condition_generator` <- function(x, ...) {
  cat("Condition generator\n")
  forms <- formals(x)
  writeLines(paste0(
    "  ",
    format(names(forms)),
    " : ",
    vapply(forms, deparse1, NA_character_)
  ))
  cat("For list of conditions use cnd::conditions()\n")
  invisible(x)
}

#' @export
`print.cnd::condition_function` <- function(x, ...) {
  cat("<<", x$class, ">>\n", sep = "")
  cat("  <type: ", x$type, ">\n", sep = "")

  if (length(x$exports)) {
    if (!is.null(x$package)) {
      exp <- paste0(x$package, "::", x$exports)
    } else {
      exp <- x$exports
    }
    cat("  <exports: ", to_string(exp), ">\n", sep = "")
  }
  if (!is.null(x$package)) {
    cat("  <package: ", x$package, ">\n", sep = "")
  }

  if (length(x$help)) {
    cat("\n")
    writeLines(clean_text(x$help, pad = 2L))
  }

  forms <- formals(x)
  if (!is.null(forms)) {
    cat("\nGenerator:\n")
    writeLines(paste0(
      "  |  ",
      format(names(forms)),
      " :",
      vapply(forms, deparse1, NA_character_)
    ))
  }

  invisible(x)
}

#' @export
`conditionMessage.cnd::condition` <- function(c) {
  exports <- attr(c, "exports")
  pkg <- attr(c, "package")

  msg <- c(fmt("<{cl}>", cl = attr(c, "condition")), c$message)

  if (length(exports)) {
    msg <- c(
      msg,
      "\n",
      "See exports for more help:",
      paste0("?", if (is.null(pkg)) "", paste0(pkg, ":::", exports))
    )
  }

  msg
}

#' @export
`as.character.cnd::condition_function` <- function(x, ...) {
  cnd(cond_as_character_condition())
}

cond_as_character_condition <- NULL
delayedAssign(
  "cond_as_character_condition",
  condition(
    "as_character_cnd_error",
    type = "error",
    package = "cnd",
    message = "
    You are trying to coerce a `cnd::condition_function` object to a character.
    Did you mean instead to call it as a function first?
    ",
    # message = c(
    #     "You are trying to coerce a `cnd::condition_function` object to a ",
    #     "character.\nDid you mean instead to call it as a function first?"
    # ),
    help = "
    You cannot coerce a `cnd::condition_function` object to a character.  This
    may have occured when trying to put a condition function through `stop()` or
    `warning`.  Instead, call the function first, then pass the result to `stop()`
    or `warning()`.

    For example:

    ```r
    # Instead of this
    stop(my_condition)

    # Do this
    stop(my_condition())
    ```
    "
  )
)

#' @export
`[.cnd::condition_function` <- function(x, i) {
  get(i, environment(x))
}

#' @export
`$.cnd::condition_function` <- function(x, i) {
  .subset2(as.list(environment(x)), i)
}


#' @export
`print.cnd::conditioned_function` <- function(x, ...) {
  print.function(as.function(as.list(x)), ...)
  cat(
    "<condition(s): ",
    to_string(vapply(attr(x, "condition"), format, NA_character_)),
    ">\n",
    sep = ""
  )
  invisible(x)
}

#' @export
`format.cnd::condition_function` <- function(x, ...) {
  paste0(x$type, "/", x$class)
}
