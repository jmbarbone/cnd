#' An example function
#'
#' @param x A value
#'
#' @section Conditions:
#'
#' `r cnd::cnd_section(example_function)`
#'
#' @export
example_function <- function(x) {
  if (!isTRUE(x)) {
    stop(cond_bad_argument())
  }
}

#' @export
#' @rdname example_function
#' @param ...,.call Condition information
#' @importFrom cnd condition
cond_bad_argument <- condition(
  "bad_argument",
  "x must be TRUE",
  type = "error",
  help = "x should be `TRUE`",
  exports = "example_function"
)
