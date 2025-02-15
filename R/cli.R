cli_on <- function() {
  requireNamespace("cli", quietly = TRUE) &&
    isTRUE(getOption("cnd.cli.on", TRUE)) &&
    cli::num_ansi_colors() > 1L
}

cli_toggle <- function(on, code) {
  op <- options(cnd.cli.on = on)
  on.exit(options(op))
  force(code)
}

cli_switch <- function(fun, ...) {
  (if (cli_on()) match.fun(fun) else base::paste0)(...)
}

# nolint: object_length_linter.
local_cli_ignore_unknown_rstudio_theme <- function() {
  op <- options(
    cli.ignore_unknown_rstudio_theme =
      getOption("cli.ignore_unknown_rstudio_theme", TRUE)
  )
  do.call(on.exit, list(options(op)), envir = parent.frame())
}

# nocov start
black   <- function(...) cli_switch(cli::col_black, ...)
blue    <- function(...) cli_switch(cli::col_blue, ...)
cyan    <- function(...) cli_switch(cli::col_cyan, ...)
green   <- function(...) cli_switch(cli::col_green, ...)
grey    <- function(...) cli_switch(cli::col_grey, ...)
magenta <- function(...) cli_switch(cli::col_magenta, ...)
red     <- function(...) cli_switch(cli::col_red, ...)
silver  <- function(...) cli_switch(cli::col_silver, ...)
white   <- function(...) cli_switch(cli::col_white, ...)
yellow  <- function(...) cli_switch(cli::col_yellow, ...)

bold  <- function(...) cli_switch(cli::style_bold, ...)
code  <- function(...) cli_switch(cli::code_highlight, ...)
# nocov end
