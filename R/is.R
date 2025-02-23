#' `is` functions for [cnd]
#'
#' @param x An object
#' @param type A specific type to check
#' @name cnd_is
#' @returns `TRUE` or `FALSE` for the test
#' @examples
#' is_condition(simpleCondition(""))
#' is_cnd_condition(simpleCondition(""))
#'
#' con <- condition("is")
#' is_condition(con)
#' is_cnd_condition(con)
#'
#' is_condition(con())
#' is_cnd_condition(con())
#'
#' is_cnd_generator(con)
#'
#' is_conditioned_function(cnd)
NULL

#' @export
#' @rdname cnd_is
is_condition <- function(x) {
  inherits(x, "condition")
}

#' @export
#' @rdname cnd_is
is_cnd_condition <- function(x) {
  inherits(x, "cnd::condition")
}

# this doesn't need to be exported
is_cnd_progenerator <- function(x) {
  inherits(x, "cnd::condition_progenitor")
}

#' @export
#' @rdname cnd_is
is_cnd_generator <- function(
  x,
  type = c("error", "warning", "message", "condition")
) {
  inherits(x, "cnd::condition_generator") && x$type %in% type
}

#' @export
#' @rdname cnd_is
is_conditioned_function <- function(x) {
  inherits(x, "cnd::conditioned_function")
}
