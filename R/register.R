
#' Register a condition
#'
#' Only register functions that are associated with a package
#'
#' @param cond A condition object
#' @param old The old condition
#' @param registry A registry name
#'
#' @noRd
register_condition <- function(cond, old = NULL, registry = NULL) {
  # See if there's already a condition created, just based on name and package.
  # The other values may change, which will be noted in the overwrite
  if (is.null(old)) {
    old <- do_find_cond(cond, force = TRUE, check = c("class", "package"))
    if (!is.null(old)) {
      old <- old[[1L]]
    }
  }

  # could play around with identical()
  if (isTRUE(all.equal(cond, old))) {
    return(invisible())
  }

  if (!is.null(old)) {
    cnd(cond_condition_overwrite(old, cond))
  }

  if (is.null(registry)) {
    pkg <- cget(cond, "package")
    if (is.null(pkg)) {
      return()
    }
    registry <- get_registry(pkg)
  } else {
    registry <- get_registry(registry)
  }

  assign(cget(cond, "class"), cond, registry)
  invisible()
}

get_registry <- function(pkg) {
  # for some reason, exists(pkg, global_registry$packages) was working weird
  if (is.null(global_registry$packages[[pkg]])) {
    assign(pkg, global_registry$new_env(), global_registry$packages)
  }

  get(pkg, global_registry$packages)
}

remove_registration <- function(pkg) {
  if (exists(pkg, global_registry$packages)) {
    rm(list = pkg, envir = global_registry$packages)
  }
}

unregister_condition <- function(cond, package = cond$package) {
  cond <- find_cond(cond)
  force(package)
  rm(list = cond$class, envir = get_registry(package))
  invisible()
}

#' Evaluate all conditions in registry
#'
#' Because conditions are delayed via [delayedAssign()], we need to force their
#' evaluation so they get assigned into the **registry** and we can retrieve
#' them for package documentation and enhancement
#'
#' @noRd
cnd_evaluate <- function() {
  invisible(lapply(parent.env(global_registry), force))
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
