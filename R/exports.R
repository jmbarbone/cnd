#' Add conditions to functions
#'
#' `[cnd_exports()]` should be used within a package's building
#' environment.
#'
#' @param env The package environment
#' @returns Nothing, called for its side-effects
#' @export
#' @examples
#' e <- new.env()
#' registry <- cnd_create_registry("EXAMPLE", env = e)
#' local(envir = e, {
#'   my_fun <- function() NULL
#'   condition(
#'     "my_condition",
#'     package = "example_package",
#'     exports = "my_fun",
#'     registry = registry
#'   )
#'   cnd_exports()
#' })
#'
#' # conditions are now added to my_fun():
#' e$my_fun
#' conditions(e$my_fun)
cnd_exports <- function(env = parent.frame()) {
  pkg <- get_package(env)
  for (cond in conditions(package = pkg)) {
    for (export in cond$exports) {
      object <- get(export, env, mode = "function")
      conditions(object, append = TRUE) <- cond
      assign(export, object, env)
    }
  }

  invisible()
}
