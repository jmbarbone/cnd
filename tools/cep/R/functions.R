#' An example function
#'
#' @section Conditions:
#' `r cnd::cnd_section(example_function)`
#' @export
example_function <- function(x) {
  if (!isTRUE(x)) {
    stop(bad_argument())
  }
}

bad_argument <- cnd::condition(
  "bad_argument",
  "x must be TRUE",
  type = "error",
  package = "cep",
  exports = "example_function"
)
