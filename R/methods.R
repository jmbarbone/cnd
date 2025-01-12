
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

  cat("  $ type   :", x$type, "\n")

  if (length(x$exports)) {
    if (!is.null(x$package)) {
      exp <- paste0(x$package, "::", x$exports)
    } else {
      exp <- x$exports
    }
    cat("  $ exports:", exp, "\n")
  }
  if (!is.null(x$package)) {
    cat("  $ package:", x$package, "\n")
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

  sprintf(
    "<%s>\r\n%s%s",
    attr(c, "condition"),
    c$message,
    if (length(exports)) {
      sprintf(
        "\nSee exports from for help: %s",
        to_string(paste0("?", if (is.null(pkg)) "", paste0(pkg, "::", exports)))
      )
  } else {
    ""
  })
}

#' @export
`as.character.cnd::condition_function` <- function(x, ...) {
  stop(cond_as_character_condition_fun())
}

cond_as_character_condition_fun <- condition(
  "as_character_cnd_error",
  type = "error",
  package = "cnd",
  message = c(
      "You are trying to coerce a `cnd::condition_function` object to a ",
      "character.\nDid you mean instead to call it as a function first?"
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
