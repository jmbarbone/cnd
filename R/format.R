
# exports -----------------------------------------------------------------

#' Format conditions
#'
#' Formats [condition] objects
#'
#' @param x A [condition] object
#' @param ... Not used
#' @param cli If `TRUE` will use formatting from [`{cli}`][cli-package]
#' @export
#' @name format-conditions
`format.cnd::condition` <- function(
    x,
    ...,
    cli = getOption("cnd.cli.on", TRUE)
) {
  a <- attributes(x)
  fmt_cond(a$package, a$condition, a$type, class(x), x$message, cli_on = cli)
}

#' @rdname format-conditions
#' @export
`format.cnd::condition_generator` <- function(
    x,
    ...,
    cli = getOption("cnd.cli.on", TRUE)
) {
  fmt_cond(x$package, x$class, x$type, cli_on = cli)
}


# helpers -----------------------------------------------------------------

fmt_cond <- function(
    package,
    class,
    type,
    classes = NULL,
    message = NULL,
    cli_on = getOption("cnd.cli.on", TRUE)
) {
  op <- options(cnd.cli.on = cli_on)
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
      grey(paste0("\n(", collapse(classes, sep = "/"), ")"))
    },
    message = if (is.null(message)) {
      ""
    } else {
      paste0("\n", italic(message))
    }
  )
}
