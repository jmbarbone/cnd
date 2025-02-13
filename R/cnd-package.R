#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

global_registry <- new.env(hash = FALSE)
class(global_registry) <- c("cnd_registry", "environment")

local(envir = global_registry, {
  new_env <- function() new.env(parent = global_registry)
  packages <- new_env()
})
