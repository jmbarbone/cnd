#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

registry <- new.env(hash = FALSE)
class(registry) <- c("cnd_registry", "environment")

local(envir = registry, {
  new_env <- function() new.env(parent = registry)
  packages <- new_env()
})
