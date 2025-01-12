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

  registry <- get_registry(pkg)
  class <- get("class", environment(cond))

  if (
    isTRUE(getOption("cnd.verbose", TRUE)) &&
    exists(class, registry)
  ) {
    cnd(cond_overwrite_registration(class))
  }

  assign(class, cond, registry)
  invisible()
}

get_registry <- function(pkg) {
  ns <- asNamespace(pkg)
  if (!exists(".__cnd::condition_registry__.", ns)) {
    if (getOption("cnd.verbose", TRUE)) {
      cnd(cond_new_registry(pkg))
    }

    assign(
      ".__cnd::condition_registry__.",
      new.env(parent = .__condition_registry__.),
      ns
    )
  }

  get(".__cnd::condition_registry__.", ns)
}

#' Condition registration environment
#'
#' @include utils.R
#' @noRd
.__condition_registry__. <- new.env()
`.__cnd::condition_registry__.` <- new.env(parent = .__condition_registry__.)


cond_overwrite_registration <- NULL
delayedAssign(
  "cond_overwrite_registration",
  condition(
    "condition_warning",
    type = "warning",
    message = function(class)
      fmt("Condition '{cl}' already exists. Overwriting.", cl = class),
    exports = "condition",
    package = "cnd",
    help = "
    Conditions created through `cnd::condition()` are registered in the package
    namespace inside the `.__cnd::condition_registry__.` object. If a condition
    with the same name already exists, it will be overwritten.  This will often
    be the case when _reloading_ a development package (e.g. with
    `devtools::load_all()`).

    This warning may be suppressed by setting `options(cnd.verbose = FALSE)`.
    "
  )
)

cond_new_registry <- NULL
delayedAssign(
  "cond_new_registry",
  condition(
    "new_registry_cnd_message",
    message = \(package) paste("Creating condition registry for", package),
    type = "message",
    package = "cnd",
    exports = "condition",
    help = "
    Condition registration is completed within a package namespace.  If you do
    not create the registry yourself, it will be created automatically when
    the resgistry is retrieved.  The name must be
    `.__cnd::condition_registry__.`, which will likely not conflict with
    other objects in your package.
    "
  )
)
