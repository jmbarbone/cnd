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

  for (cond in conditions(pkg)) {
    for (export in cond$exports) {
      add_condition(cond$exports, cond, env)
    }
  }

  invisible()
}

add_condition <- function(export, condition, env) {
  object <- get(export, env, mode = "function")
  attr(object, "conditions") <- c(attr(object, "conditions"), condition)

  if (!is_conditioned_function(object)) {
    class(object) <- c("cnd::conditioned_function", class(object))
  }

  assign(export, object, env)
}
