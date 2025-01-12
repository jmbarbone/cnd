#' @keywords internal
#' @import S7
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

capsule <- new.env(hash = FALSE)
class(capsule) <- c("cnd_capsule", "environment")

encapsulate <- function(expr, env = capsule) {
  expr <- substitute(expr)
  eval(expr, env)
}

# global variable-ish
`..` <- NULL
