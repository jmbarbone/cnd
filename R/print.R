
# exports -----------------------------------------------------------------

#' @export
`print.cnd::condition_generator` <- function(x, ...) {
  local_cli_ignore_unknown_rstudio_theme()
  cat(blue(bold("cnd::condition_generator\n")))
  cat(format(x))
  print_generator(x$message)

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
  cli_text(
    "For a list of conditions: {.run cnd::conditions()}",
    "For a list of conditions: `cnd::conditions()`",
  )
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
  cat(format(x))
  invisible(x)
}


# helpers -----------------------------------------------------------------

print_conditions <- function(x) {
  local_cli_ignore_unknown_rstudio_theme()

  conds <- attr(x, "conditions") %||% return()

  cat("\n", bold("condition(s)"), "\n", sep = "")
  clean <- override_cli("off", vapply(conds, format, NA_character_))
  if (!cli_on()) {
    cat(clean, sep = "\n")
    return()
  }

  fmt <- override_cli("on", vapply(conds, format, NA_character_))
  code <- sprintf("cnd::cond(\"%s\")", clean)
  text <- sprintf("  {.run [%s](%s)}", fmt, code)

  for (i in seq_along(text)) {
    cli_text(text[i], fmt[i])
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

