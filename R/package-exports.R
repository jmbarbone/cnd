#' Add conditions to functions
#'
#' `[cnd_exports()]` should be used within a package's building
#' environment.
#'
#' @param env The package environment
#' @returns Nothing, called for its side-effects
#' @export
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
