

# print -------------------------------------------------------------------

#' @export
`print.cnd::condition_function` <- function(x, ...) {
  cat("<", format(x), ">\n", sep = "")

  forms <- formals(x$message)
  if (!is.null(forms)) {
    values <- vapply(forms, deparse1, NA_character_)
    types <- paste0("<", vapply(forms, typeof, NA_character_), "> ")

    cat(
      "\ngenerator:",
      paste0("\n  $ ", format(names(forms)), ": ", types, values),
      "\n",
      sep = ""
    )
  }

  if (length(x$help)) {
    cat("\n")
    writeLines(clean_text(x$help))
  }

  if (length(x$exports)) {
    cat(
      "\nexports:",
      to_string(paste0("\n  ", x$package, "::", x$exports)),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}

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
`print.cnd::condition` <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}


# format ------------------------------------------------------------------

#' @export
`format.cnd::condition` <- function(x, ...) {
  pkg <- attr(x, "package")
  fmt(
    "<{pkg}{class}/{type}>\n{message}",
    pkg = if (is.null(pkg)) "" else paste0(pkg, ":"),
    class = sub("^.*:+", "", attr(x, "condition")),
    type = attr(x, "type"),
    message = collapse(x$message)
  )
}

#' @export
`format.cnd::condition_function` <- function(x, ...) {
  fmt(
    "{pkg}{class}/{type}",
    pkg = if (is.null(x$package)) "" else paste0(x$package, ":"),
    type = x$type,
    class = sub("^.*:+", "", x$class)
  )
}
