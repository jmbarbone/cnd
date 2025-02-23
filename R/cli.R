# All functions from {cli} are indirectly called.  They are wrapped in a check
# for the package itself and for whether or not ansi colors are detected.  The
# default is to check for the package and the colors, but this can be overridden
# by setting the option `cnd.cli.override` to either "on" or "off".

cli_on <- function() {
  switch(
    getOption("cnd.cli.override", "none"),
    on = TRUE,
    off = FALSE,
    requireNamespace("cli", quietly = TRUE) && cli::num_ansi_colors() > 1L
  )
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
black   <- function(...) cli_fun("col_black", ...) # fmt: skip
blue    <- function(...) cli_fun("col_blue", ...) # fmt: skip
cyan    <- function(...) cli_fun("col_cyan", ...) # fmt: skip
green   <- function(...) cli_fun("col_green", ...) # fmt: skip
grey    <- function(...) cli_fun("col_grey", ...) # fmt: skip
magenta <- function(...) cli_fun("col_magenta", ...) # fmt: skip
red     <- function(...) cli_fun("col_red", ...) # fmt: skip
silver  <- function(...) cli_fun("col_silver", ...) # fmt: skip
white   <- function(...) cli_fun("col_white", ...) # fmt: skip
yellow  <- function(...) cli_fun("col_yellow", ...) # fmt: skip
bold    <- function(...) cli_fun("style_bold", ...) # fmt: skip
italic  <- function(...) cli_fun("style_italic", ...) # fmt: skip
code    <- function(...) cli_fun("code_highlight", ...) # fmt: skip
# nocov end

# handled retains .envir and just prints directly
cli_text <- function(..., .envir = parent.frame()) {
  cli_switch(
    cli::cli_text(..., .envir = .envir),
    cat(..., "\n", sep = "")
  )
}
