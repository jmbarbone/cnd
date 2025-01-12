is_condition <- function(x) {
  inherits(x, "condition")
}

is_cnd_condition <- function(x) {
  inherits(x, "cnd::condition")
}

#' @export
`is.cnd::condition` <- function(x) {
  is_cnd_condition(x)
}

is_cnd_generator <- function(x) {
  inherits(x, "cnd::condition_generator")
}

#' @export
`is.cnd::condition_generator` <- function(x) {
  is_cnd_generator(x)
}

is_cnd_function <- function(x, type = c("error", "warning", "message")) {
  inherits(x, "cnd::condition_function") && x$type %in% type
}

#' @export
`is.cnd::condition_function` <- function(x) {
  is_cnd_function(x)
}

is_conditioned_function <- function(x) {
  inherits(x, "cnd::conditioned_function")
}

#' @export
`is.cnd::conditioned_function` <- function(x) {
  is_conditioned_function(x)
}
