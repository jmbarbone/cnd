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
  invisible(lapply(parent.env(global_registry), force))
}

# TODO document op.cnd
op.cnd <- list( # nolint: object_name_linter.
  cnd.condition.silent = FALSE,
  cnd.call = TRUE
)

.onLoad <- function(libname, pkgname) {
  options(op.cnd[!names(op.cnd) %in% names(options())]) # nocov
}
