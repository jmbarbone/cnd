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
