#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

registry <- new.env(hash = FALSE)
class(registry) <- c("cnd_registry", "environment")

encapsulate <- function(expr, env = registry)  {
  expr <- substitute(expr)
  eval(expr, env)
}

encapsulate({
  new_env <- function() {
    new.env(parent = registry)
  }

  packages <- new_env()
})

# global variable-ish
`..` <- NULL
