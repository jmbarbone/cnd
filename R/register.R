

#' Register conditions
#'
#' `[register_conditions()]` should be used within a package's `.onLoad()`
#' function to modify functions to include conditions
#'
#' @param env The package environment
#' @returns Nothing, called for its side-effects
#' @export
register_conditions <- function(env = parent.frame()) {
  pkg <- get_package(env)

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
#' @param old the old condition
#' @param ... Unused
#' @param env The parent environment to register the condition
#'
#' @include utils.R
#' @noRd
register_condition <- function(cond, old, ..., env = parent.frame()) {
  # could play around with identical()
  if (isTRUE(all.equal(cond, old))) {
    return(invisible())
  }

  # S3 methods may not be available when building the cnd package
  pkg <- get0("package", environment(cond))

  if (is.null(pkg)) {
    return()
  }

  if (!is.null(old)) {
    cnd(cond_condition_overwrite(old))
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

cond_condition_overwrite <- NULL
delayedAssign(
  "cond_condition_overwrite",
  condition(
    "condition_overwrite",
    type = "warning",
    package = "cnd",
    message = \(cond) fmt(
      "A condition with the class name {cls} already exists in {pkg} and will",
      " be overwritten",
      cls = cond$class,
      pkg = conditions(class = cond$class)[[1L]]$pkg
    )
  )
)
