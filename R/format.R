# exports -----------------------------------------------------------------

#' Format conditions
#'
#' Formats [condition] objects
#'
#' @param x A [condition] object
#' @param ... Not used
#' @param cli If `TRUE` will use formatting from [cli][cli::cli-package].
#'   Default uses an option, `"cnd.cli.override"`, if available, otherwise
#'   checks that `cli` is installed and ansi colors are available.
#' @export
#' @returns A `character` vector
#' @examples
#' format(condition("foo"))
#' @name format-conditions
`format.cnd::condition` <- function(
  x,
  ...,
  cli = getOption("cnd.cli.override")
) {
  a <- attributes(x)
  fmt_cond(
    package = a$package,
    class = a$condition,
    type = a$type,
    classes = class(x),
    message = x$message,
    cli_override = cli
  )
}

#' @rdname format-conditions
#' @export
`format.cnd::condition_generator` <- function(
  x,
  ...,
  cli = getOption("cnd.cli.override")
) {
  fmt_cond(
    package = x$package,
    class = x$class,
    type = x$type,
    cli_override = cli
  )
}


# helpers -----------------------------------------------------------------

fmt_cond <- function(
  package,
  class,
  type,
  classes = NULL,
  message = NULL,
  cli_override = getOption("cnd.cli.override")
) {
  op <- options(cnd.cli.on = cli_override)
  on.exit(options(op), add = TRUE)

  fmt(
    "{package}{class}/{type}{classes}{message}",
    package = if (is.null(package)) {
      ""
    } else {
      paste0(bold(package), ":")
    },
    class = sub("^.*:+", "", class),
    type = switch(
      type,
      error = red,
      warning = yellow,
      message = blue,
      condition = silver
    )(type),
    classes = if (is.null(classes)) {
      ""
    } else {
      paste0("\n", grey(paste0("(", collapse(classes, sep = "/"), ")")))
    },
    message = if (is.null(message)) {
      ""
    } else {
      paste0("\n", collapse(italic(message), sep = "\n"))
    }
  )
}
