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

filter2 <- function(x, fun, ...) {
  fun <- match.fun(fun)
  x[which(vapply(x, fun, NA, ...))]
}

fmt <- function(...) {
  params <- list(...)
  nms <- names(params)
  if (is.null(nms)) {
    return(paste0(unlist(params, use.names = FALSE), collapse = ""))
  }

  lines <- names(params) == ""
  text <- unlist(params[lines])
  params <- params[!lines]
  names <- names(params)

  for (i in seq_along(params)) {
    text <- gsub(paste0("{", names[i], "}"), params[i], text, fixed = TRUE)
  }

  fmt(text)
}


clean_text <- function(x, pad = 0L) {
  x <- as.character(x)
  x <- clean_padding(x, pad)

  while (x[1L] == "") {
    x <- x[-1L]
  }

  while (x[n <- length(x)] == "") {
    x <- x[-n]
  }

  x
}

clean_padding <- function(x, pad = 0L) {
  pad <- as.integer(pad)
  text <- unlist(strsplit(x, "\n", fixed = TRUE))
  ok <- text != ""

  ns <- attr(regexpr("^[[:space:]]+", text[ok], perl = TRUE), "match.length")
  m <- min(ns)

  if (m == 0) {
    return(text)
  }

  text[ok] <- substr(text[ok], m + 1L, nchar(text[ok]))
  text[ok] <- paste0(strrep(" ", pad), text[ok])
  text
}
