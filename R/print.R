# exports -----------------------------------------------------------------

#' @export
`print.cnd::condition_generator` <- function(x, ...) {
  local_cli_ignore_unknown_rstudio_theme()
  cat(blue(bold("cnd::condition_generator\n")))
  cat(format(x), "\n")
  print_generator(x$message)
  print_help(x$help)
  print_exports(x$exports, x$package)
  invisible(x)
}

#' @export
`print.cnd::condition_progenitor` <- function(x, ...) {
  cat(blue(bold("cnd::condition_progenitor\n")))

  print_generator(x)
  print_conditions(x)

  cat("\n")
  cli_switch(
    cli_text("For a list of conditions: {.run cnd::conditions()}"),
    cat("For a list of conditions: `cnd::conditions()`\n")
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

  # should only be called when cli is on
  fmt <- override_cli("on", vapply(conds, format, NA_character_))
  code <- sprintf("cnd::cond(\"%s\")", clean)
  text <- sprintf("  {.run [%s](%s)}", fmt, code)
  for (line in text) {
    cli_text(line)
  }
  invisible()
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
      grey("\n  $ "),
      grey(format(names(forms))),
      " : ",
      cyan(types),
      values
    ),
    "\n",
    sep = ""
  )
}

print_help <- function(help) {
  if (length(help)) {
    cat(bold("\nhelp\n"))
    cat(grey(clean_text(help)), "\n")
  }
}

print_exports <- function(exports, package) {
  if (length(exports)) {
    cat(
      bold("\nexports\n"),
      paste0(
        "  ",
        code(paste0(package, "::", exports, "()"))
      ),
      "\n",
      sep = ""
    )
  }
}
