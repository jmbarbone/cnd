

#' Register conditions
#'
#' `[register_conditions()]` should be used within a package's `.onLoad()`
#' function to modify functions to include conditions
#'
#' @param pkg The package name
#' @returns Nothing, called for its side-effects
#' @export
register_conditions <- function(env = parent.frame()) {
  pkg <- if (identical(env, parent.env(registry))) {
    "cnd"
  } else {
    get_package(env)
  }

  add_condition <- function(export, condition) {
    object <- get(export, env, mode = "function")
    attr(object, "condition") <- c(attr(object, "condition"), condition)

    if (!is_conditioned_function(object)) {
      class(object) <- c("cnd::conditioned_function", class(object))
    }

    assign(export, object, env)
  }

  for (cond in conditions(pkg)) {
    for (export in cond$exports) {
      add_condition(cond$exports, cond)
    }
  }

  invisible()
}

#' Register a condition
#'
#' Only register functions that are associated with a package
#'
#' @param cond A condition object
#' @param env The parent environment to register the condition
#'
#' @include utils.R
#' @noRd
register_condition <- function(cond, env = parent.frame()) {
  # S3 methods may not be available when building the cnd package
  pkg <- get0("package", environment(cond))

  if (is.null(pkg)) {
    return()
  }

  assign(get("class", environment(cond)), cond, get_registry(pkg))
  invisible()
}

get_registry <- function(pkg) {
  # for some reason, exists(pkg, registry$packages) was working weird
  if (is.null(registry$packages[[pkg]])) {
    assign(pkg, registry$new_env(), registry$packages)
  }

  get(pkg, registry$packages)
}

