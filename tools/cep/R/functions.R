#' An example function
#'
#' @section Conditions:
#'
#' `r cnd::cnd_section("example_function")`
#'
#' @export
example_function <- function(x) {
  if (!isTRUE(x)) {
    stop(bad_argument())
  }
}

#' @export
#' @rdname example_function
bad_argument <- cnd::condition(
  "bad_argument",
  "x must be TRUE",
  type = "error",
  help = "x should be `TRUE`",
  exports = "example_function"
)
