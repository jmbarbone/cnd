#' `is` functions for {cnd}
#'
#' @param x An object
#' @param type A specific type to check
#' @name cnd_is
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

#' @export
#' @rdname cnd_is
is_cnd_generator <- function(x) {
  inherits(x, "cnd::condition_generator")
}

#' @export
#' @rdname cnd_is
is_cnd_function <- function(x, type = c("error", "warning", "message")) {
  inherits(x, "cnd::condition_function") && x$type %in% type
}

#' @export
#' @rdname cnd_is
is_conditioned_function <- function(x) {
  inherits(x, "cnd::conditioned_function")
}
