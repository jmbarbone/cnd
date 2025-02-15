

# print -------------------------------------------------------------------

#' @export
`print.cnd::condition_generator` <- function(x, ...) {
  local_cli_ignore_unknown_rstudio_theme()
  cat(blue(bold("cnd::condition_generator\n")))
  cat("<", format(x), ">\n", sep = "")
  print_generator(x$message)
  cat("\n")
  if (length(x$help)) {
    cat(bold("help\n"))
    cat(grey(clean_text(x$help)))
  }

  if (length(x$exports)) {
    cat(
      bold("\nexports\n"),
      paste0(
        "  ",
        code(paste0(x$package, "::", x$exports, "()"))
      ),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}

#' @export
`print.cnd::condition_progenitor` <- function(x, ...) {
  cat(blue(bold("cnd::condition_progenitor\n")))
  # cat(
  #   "This special function creates `cnd::condition_generator` objects",
  #   "which are used to create conditions.\n"
  # )
  print_generator(x)
  print_conditions(x)
  cat("\n")
  cli::cli_text("For a list of conditions: {.run cnd::conditions()}")
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
  cat(blue(format(x)), "\n", sep = "")
  invisible(x)
}

print_conditions <- function(x) {
  local_cli_ignore_unknown_rstudio_theme()

  conds <- attr(x, "conditions") %||% return()

  cat("\n", bold("condition(s)"), "\n", sep = "")
  clean <- cli_toggle(FALSE, vapply(conds, format, NA_character_))
  fmt <- cli_toggle(TRUE, vapply(conds, format, NA_character_))
  code <- sprintf("cnd::cond(\"%s\")", clean)
  text <- sprintf("  {.run [%s](%s)}", fmt, code)
  for (line in text) {
    cli::cli_text(line)
  }
}

print_generator <- function(x) {
  forms <- formals(x) %||% return()

  values <- vapply(forms, deparse1, NA_character_)
  types <- paste0("<", vapply(forms, typeof, NA_character_), "> ")
  types[types == "<NULL> "] <- ""
  values[values == "NULL"] <- grey("NULL")

  cat(
    "\n",
    bold("generator"),
    paste0(
      grey("\n  $ "), grey(format(names(forms))), " : ",
      cyan(types),
      values
    ),
    "\n",
    sep = ""
  )
}

# format ------------------------------------------------------------------

#' @export
`format.cnd::condition` <- function(x, ...) {
  pkg <- attr(x, "package")
  fmt(
    "<{pkg}{class}/{type}::>\n{message}",
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
    pkg = if (is.null(x$package)) "" else paste0(magenta(x$package), ":"),
    class = magenta(sub("^.*:+", "", x$class)),
    type = switch(
      x$type,
      error = red,
      warning = yellow,
      message = blue,
      condition = white
      )(x$type)
  )
}
