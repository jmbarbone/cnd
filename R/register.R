
#' Register a condition
#'
#' Only register functions that are associated with a package
#'
#' @param cond A condition object
#' @param old the old condition
#' @param ... Unused
#' @param env The parent environment to register the condition
#'
#' @noRd
register_condition <- function(cond, old, ..., env = parent.frame()) {
  # See if there's already a condition created, just based on name and package.
  # The other values may change, which will be noted in the overwrite
  old <- conditions(
    class = cget(cond, "class"),
    package = cget(cond, "package")
  )[[1L]]
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
    cnd(cond_condition_overwrite(old, cond))
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

#' Evaluate all conditions in registry
#'
#' Because conditions are delayed via [delayedAssign()], we need to force their
#' evaluation so they get assigned into the **registry** and we can retrieve
#' them for package documentation and enhancement
#'
#' @noRd
cnd_evaluate <- function() {
  invisible(lapply(parent.env(registry), force))
}

cond_condition_overwrite <- NULL
delayedAssign(
  "cond_condition_overwrite",
  condition(
    "condition_overwrite",
    type = "warning",
    package = "cnd",
    # nolint next: brace_linter.
    message = \(old, new) fmt(
      "A condition with the class name '{cls}' already exists in '{pkg}' and",
      " will be overwritten{diff}",
      cls = old$class,
      pkg = old$package,
      diff = paste0("\n   ", all.equal(old, new), collapse = "")
    )
  )
)
