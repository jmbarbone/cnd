get_package <- function(env = parent.frame(2L)) {
  # used specifically in condition()
  top <- topenv(env)
  if (isNamespace(top)) {
    unname(getNamespaceName(top))
  }
}

to_string <- function(x) {
  paste0(x, collapse = ", ")
}

}




}


}

filter2 <- function(x, fun, ...) {
  fun <- match.fun(fun)
  x[which(vapply(x, fun, NA, ...))]
}
