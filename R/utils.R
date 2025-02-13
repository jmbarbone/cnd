get_package <- function(env = parent.frame(2L)) {
  if (is_cnd(env)) {
    return("cnd")
  }

  # used specifically in condition()
  top <- topenv(env)
  if (isNamespace(top)) {
    unname(getNamespaceName(top))
  }
}

is_cnd <- function(env) {
  identical(as.environment(env), parent.env(registry))
}

to_string <- function(x) {
  collapse(x, sep = ", ")
}

collapse <- function(..., sep = "") {
  paste0(unlist(c(...)), collapse = sep)
}

filter2 <- function(x, fun, ...) {
  fun <- match.fun(fun)
  x[which(vapply(x, fun, NA, ...))]
}

set_class <- function(x, value) {
  class(x) <- value
  x
}

add_class <- function(x, value) {
  if (!inherits(x, value)) {
    class(x) <- c(value, class(x))
  }
  x
}

remove_class <- function(x, value) {
  set_class(x, filter2(class(x), function(cl) !cl %in% value))
}

# yeah, just going to save over this one
attr <- function(x, which) {
  base::attr(x, which, exact = TRUE)
}

fmt <- function(..., .sep = "") {
  # this is actually pretty neat, and maybe will go into `{fuj}`
  params <- list(...)
  nms <- names(params)
  if (is.null(nms)) {
    if (isTRUE(is.na(.sep))) {
      return(unlist(params))
    }
    return(collapse(params, sep = .sep))
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
  text <- unlist(strsplit(x, "\n", fixed = TRUE))
  ok <- text != ""

  ns <- attr(regexpr("^[[:space:]]+", text[ok], perl = TRUE), "match.length")
  m <- min(ns)

  if (m == 0) {
    return(text)
  }

  text[ok] <- substr(text[ok], m + 1L, nchar(text[ok]))
  if (is.numeric(pad)) {
    pad <- as.integer(pad)
    text[ok] <- paste0(strrep(" ", pad), text[ok])
  } else {
    text[ok] <- paste0(pad, text[ok])
  }
  text
}

match_arg <- function(arg, choices) {
  if (missing(choices)) {
    parent <- sys.parent()
    fargs <- formals(sys.function(parent))
    choices <- eval(
      fargs[[as.character(substitute(arg))]],
      envir = sys.frame(parent)
    )
  }

  ok <- match(arg, choices, nomatch = 0L)
  ok <- ok[ok > 0L][1L]

  if (is.na(ok)) {
    value <- arg
    arg <- deparse1(substitute(arg))
    cnd(cond_match_arg(arg, value, choices))
  }

  choices[ok]
}

cond_match_arg <- NULL
delayedAssign(
  "cond_match_arg",
  condition(
    "match_arg",
    type = "error",
    package = "cnd",
    # nolint next: brace_linter.
    message = \(arg, value, choices) fmt(
      "Argument '{arg}' not valid\n",
      "value  : {value}\n",
      "choices: {choices}",
      arg = arg,
      value = value,
      choices = collapse(choices, sep = ", ")
    )
  )
)
