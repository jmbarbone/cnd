

# print -------------------------------------------------------------------

#' @export
`print.cnd::condition_generator` <- function(x, ...) {
  cat("Condition generator\n")
  cat("<", format(x), ">\n", sep = "")

  print_generator(x$message)
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
`print.cnd::condition_progenitor` <- function(x, ...) {
  cat("Condition progenitor\n")
  print_generator(x)
  cat("\n")
  print_conditions(x)
  cat("\nFor list of conditions use cnd::conditions()\n")
  invisible(x)
}

#' @export
`print.cnd::conditioned_function` <- function(x, ...) {
  y <- x
  attr(y, "conditions") <- NULL
  class(y) <- setdiff(class(y), "cnd::conditioned_function")
  if (identical(class(y), "function")) {
    class(y) <- NULL
  }
  print(y, ...)
  print_conditions(x)
  invisible(x)
}

#' @export
`print.cnd::condition` <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

print_conditions <- function(x) {
  cat(
    "<condition(s): ",
    to_string(vapply(attr(x, "conditions"), format, NA_character_)),
    ">\n",
    sep = ""
  )
}

print_generator <- function(x) {
  forms <- formals(x)
  if (is.null(forms)) {
    return()
  }

  values <- vapply(forms, deparse1, NA_character_)
  types <- paste0("<", vapply(forms, typeof, NA_character_), "> ")
  types[types == "<NULL> "] <- ""
  cat(
    "generator:",
    paste0("\n  $ ", format(names(forms)), ": ", types, values),
    "\n",
    sep = ""
  )
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
`format.cnd::condition_generator` <- function(x, ...) {
  fmt(
    "{pkg}{class}/{type}",
    pkg = if (is.null(x$package)) "" else paste0(x$package, ":"),
    type = x$type,
    class = sub("^.*:+", "", x$class)
  )
}
