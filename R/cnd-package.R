#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Evaluate all conditions in registry
#'
#' Because conditions are delayed via [delayedAssign()], we need to force their
#' evaluation so they get assigned into the **registry** and we can retrieve
#' them for package documentation and enhancement
#'
#' @noRd
cnd_evaluate <- function() {
  # this registry has to be force so that we get the .__CND_REGISTRY__.
  # environment created.  Otherwise, we hit a fun error when we try to grab all
  # the names from our environment:
  #
  #> Caused by error in `as.list.environment()`:
  #> ! attempt to set index 96/96 in SET_VECTOR_ELT
  force(.__CND_PACKAGE_REGISTRY__.)
  invisible(lapply(as_list_env(.cnd_env, all = TRUE), force))
}

# TODO document op.cnd
# nolint next: object_name_linter.
op.cnd <- list(
  cnd.cli.override = NULL,
  cnd.condition.message = "verbose",
  cnd.call = TRUE
)

.onLoad <- function(libname, pkgname) {
  options(op.cnd[!names(op.cnd) %in% names(options())]) # nocov
}

# assigning to a difference name so we can force the evaluation when we try to
# find any conditions
.__CND_PACKAGE_REGISTRY__. <- NULL
delayedAssign(
  ".__CND_PACKAGE_REGISTRY__.",
  registrar$create(
    registry = "cnd",
    overwrite = TRUE,
    name = ".__CND_REGISTRY__.",
    env = .cnd_env
  )
)
