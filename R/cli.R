# All functions from {cli} are indirectly called.  They are wrapped in a check
# for the package itself and for whether or not ansi colors are detected.  The
# default is to check for the package and the colors, but this can be overridden
# by setting the option `cnd.cli.override` to either "on" or "off".

cli_on <- function() {
  switch(
    getOption("cnd.cli.override", "none"),
    true = ,
    on = TRUE,
    false = ,
    off = FALSE,
    NULL
  ) %||%
    (requireNamespace("cli", quietly = TRUE) && cli::num_ansi_colors() > 1L)
}

override_cli <- function(status = c("on", "off"), expr) {
  status <- match_arg(status)
  op <- options(cnd.cli.override = status)
  on.exit(options(op))
  force(expr)
}

cli_switch <- function(on, off = NULL) {
  switch(2L - cli_on(), on, off)
}

# nolint next: object_length_linter.
local_cli_ignore_unknown_rstudio_theme <- function() {
  op <- options(
    cli.ignore_unknown_rstudio_theme = getOption(
      "cli.ignore_unknown_rstudio_theme",
      TRUE
    )
  )
  do <- function() options(op)
  do.call(on.exit, list(do()), envir = parent.frame())
}

# nolint next: object_name_linter.
cli_fun <- function(cli, ..., ..otherwise = base::paste0) {
  cli_switch(
    get(cli, asNamespace("cli"), mode = "function"),
    match.fun(..otherwise)
  )(...)
}

# nocov start
# fmt: skip
# nolint next: brace_linter.
{
  black   <- function(...) cli_fun("col_black", ...)
  blue    <- function(...) cli_fun("col_blue", ...)
  cyan    <- function(...) cli_fun("col_cyan", ...)
  green   <- function(...) cli_fun("col_green", ...)
  grey    <- function(...) cli_fun("col_grey", ...)
  magenta <- function(...) cli_fun("col_magenta", ...)
  red     <- function(...) cli_fun("col_red", ...)
  silver  <- function(...) cli_fun("col_silver", ...)
  white   <- function(...) cli_fun("col_white", ...)
  yellow  <- function(...) cli_fun("col_yellow", ...)
  bold    <- function(...) cli_fun("style_bold", ...)
  italic  <- function(...) cli_fun("style_italic", ...)
  code    <- function(...) cli_fun("code_highlight", ...)
}
# nocov end

# handled retains .envir and just prints directly
cli_text <- function(..., .envir = parent.frame()) {
  cli_switch(
    cli::cli_text(..., .envir = .envir),
    cat(..., "\n", sep = "")
  )
}
