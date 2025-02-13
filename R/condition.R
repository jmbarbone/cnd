#' Conditions
#'
#' Conditions
#'
#' @description [condition()] is used to create a new condition function that
#' itself returns a new `condition`.
#'
#' [conditions()] retrieves all conditions based on search values.  The
#' parameters serve as filtering arguments.
#'
#' @param class The name of the new class
#' @param message The message to be displayed when the condition is called
#' @param type The type of condition: error, warning, or message
#' @param exports The exported functions to be displayed when the condition is
#'   called
#' @param help The help message to be displayed for the condition function
#' @param package The package to which the condition belongs
#' @param register Controls registration checks
#'
#' @returns
#' - [condition()] a `cnd::condition_function` object
#' - [conditions()] a `list` of all conditions
#' @export
#' @examples
#' # create a new condition:
#' cond_bad_value <- condition("bad_value")
#'
#' # use the condition
#' try(stop(cond_bad_value()))
#' try(cnd(cond_bad_value()))
#'
#' # dynamic messages:
#' cond_class_error <- condition(
#'   "class_error",
#'   message = \(x) paste("class cannot be", toString(class(x)))
#' )
#' try(stop(cond_class_error(list())))
condition <- function(
    class,
    message = NULL,
    type = c("error", "warning", "message", "condition"),
    package = get_package(),
    exports = NULL,
    help = NULL,
    register = !is.null(package)
) {
  if (nargs() == 1L) {
    found <- do_find_cond(class)
    if (length(found) == 1) {
      return(found)
    }
  }

  force(package)
  force(register)

  validate_condition(class = class, exports = exports, help = help)

  type <- match_arg(type)

  original_class <- class
  if (is.null(package)) {
    if (!is.null(exports)) {
      cnd(cond_no_package_exports())
      exports <- NULL
    }
  } else {
    class <- paste(package, class, sep = ":")
  }

  if (is.null(message)) {
    message <- function() "there was an error"
  } else if (is.character(message)) {
    message <- as.function(list(collapse(message)))
  } else if (!is.function(message)) {
    cnd(cond_condition_bad_message())
  }

  if (!is.null(help)) {
    help <- collapse(help)
  }

  # setting up an environment to track additional fields for

  condition_env <- registry$new_env()
  environment(message) <- condition_env
  assign("message", message, condition_env)
  assign("exports", exports, condition_env)
  assign("package", package, condition_env)
  assign("class", class, condition_env)
  assign(".class", original_class, condition_env)
  assign("type", type, condition_env)
  assign("help", help, condition_env)

  res <- local(envir = condition_env, {
    condition_function <- function() {}
    body(condition_function) <- substitute({
      # nolint next: object_usage_linter.
      call <- sys.call(sys.parent(.ncall + 1L))

      # nolint next: object_usage_linter.
      params <- as.list(match.call())[-1L]
      params <- params[names(params) != ".ncall"]
      params <- lapply(params, eval.parent, 2L)

      # nolint next: object_usage_linter.
      cond <- list(
        message = clean_text(do.call(message, params)),
        call = call
      )

      cond <- set_class(
        cond,
        unique(c(class, "cnd::condition", type, "condition"))
      )

      attr(cond, "help") <- help
      attr(cond, "package") <- package
      attr(cond, "exports") <- exports
      attr(cond, "condition") <- class
      attr(cond, "type") <- type
      cond
    })
    condition_function
  })

  lockEnvironment(condition_env)

  formals(res) <- c(formals(message), .ncall = 0L)
  base::class(res) <- c("cnd::condition_function", "function")

  if (register) {
    register_condition(res)
  }

  res
}

find_cond <- function(x, ...) {
  found <- do_find_cond(x, ...)

  if (is_cnd_function(found)) {
    return(found)
  }

  switch(
    length(found) + 1L,
    stop("no condition found"), # TODO replace with custom condition
    return(found[[1L]])
  )

  warning("multiple conditions found")
  found[[1L]]
}

do_find_cond <- function(
    x,
    force = FALSE,
    check = c("package", "class", "type")
) {
  check <- intersect(check, eval(formals(do_find_cond)$check))
  stopifnot(!identical(check, character())) # internal error

  if (is_cnd_function(x)) {
    if (!force) {
      return(x)
    }

    package <- cget(x, "package")
    class <- cget(x, ".class")
    type <- cget(x, "type")
  } else {
    package <- str_extract(x, "^.*(?=:.*)")
    class <- gsub("^.*:|/.*$", "", x)
    type <- str_extract(x, "(?<=/).*$")
  }

  args <- list(package = package, class = class, type = type)
  args <- args[match(check, names(args))]
  do.call(conditions, args)
}

str_extract <- function(x, pattern, perl = TRUE, ...) {
  m <- regexpr(pattern, x, perl = TRUE, ...)
  res <- regmatches(x, m)
  if (length(res)) res else NULL
}

#' @export
#' @rdname condition
#' @param ... Input argument.  If a function is passed, then defaults to passing
#'   `..1` to `fun`; otherwise defaults to passing `..1` to `package`
#' @param fun if a function is passed, then retrieves the `"conditions"`
#'   attribute
conditions <- function(
    ...,
    class = NULL,
    type = NULL,
    package = NULL,
    fun = NULL
) {

  dot_n <- ...length()

  if (dot_n) {
    if (dot_n > 1) {
      warning("only the first ... argument is used")
    }

    if (is.function(..1)) {
      fun <- fun %||% ..1
    } else {
      package <- package %||% ..1
    }
  }

  if (!is.null(fun)) {
    fun <- match.fun(fun)
    return(attr(fun, "conditions"))
  }

  conds <- Reduce("c", lapply(registry$packages, as.list))
  terms <- list(package = package, .class = class, type = type)
  terms <- filter2(terms, Negate(is.null))

  for (i in seq_along(terms)) {
    conds <- filter2(conds, \(cond) cget(cond, names(terms)[i]) == terms[[i]])
  }

  if (!length(conds)) {
    return()
  }

  unname(conds)
}

#' @export
#' @rdname condition
#' @param condition A condition object
#' @returns
#' - [cnd()] is a wrapper for calling [stop()], [warning()], or [message()]
cnd <- function(condition) {
  if (!is_cnd_condition(condition)) {
    cnd(cond_cnd_class())
  }

  switch(
    attr(condition, "type"),
    error = stop(condition), # maybe `error()` should be the name
    warning = warning(condition),
    message = message(condition),
    condition = {
      signalCondition(condition)
      cat(condition$message, "\n")
    }
  )
}

#' @export
#' @rdname condition
#' @param x An object
#' @param ... Additional arguments passed to methods
#' @param value A `condition`
`conditions<-` <- function(x, ..., value) {
  UseMethod("conditions<-")
}

#' @export
#' @rdname condition
#' @param append If `TRUE`, adds to the list of `conditions`
`conditions<-.function` <- function(x, append = FALSE, ..., value) {
  if (is.null(value)) {
    x <- remove_conditions(x)
    x <- remove_class(x, "cnd::conditioned_function")
    return(x)
  }

  attr(x, "conditions") <- c(
    if (append) attr(x, "conditions"),
    if (is.list(value)) value else list(value)
  )

  if (!is_conditioned_function(x)) {
    class(x) <- c("cnd::conditioned_function", class(x))
  }

  x
}

remove_conditions <- function(x) {
  attr(x, "conditions") <- NULL
  x
}

validate_condition <- function(class, exports, help) {
  # reset problems
  problems <- character()
  problem <- function(...) problems <<- c(problems, ...)

  if (!is.character(class)) {
    problem("`class` must be a character vector")
  }

  if (length(class) != 1L) {
    problem("`class` must be a single character string")
  } else if (!grepl("^[a-z0-9_.]+$", class, ignore.case = TRUE)) {
    problem(
      "`class` must only contain letters, numbers, underscores, or periods"
    )
  }

  if (!(is.null(exports) || is.character(exports))) {
    problem("`exports` must be NULL or a character vector")
  }

  if (!(is.null(help) || is.character(help))) {
    problem("`help` must be NULL or a character vector")
  }

  if (length(problems)) {
    cnd(cond_condition_invalid(problems, .ncall = 1L))
  }
}


# methods -----------------------------------------------------------------

#' @export
`[.cnd::condition_function` <- function(x, i) {
  cget(x, i)
}

cget <- function(x, field) {
  get(field, environment(x))
}

#' @export
`$.cnd::condition_function` <- function(x, i) {
  .subset2(as.list(environment(x), all.names = TRUE), i)
}


#' @export
`conditionMessage.cnd::condition` <- function(c) {
  exports <- attr(c, "exports")
  pkg <- attr(c, "package")

  msg <- c(
    fmt("<{cl}>", cl = attr(c, "condition")),
    collapse(c$message, sep = "\n")
  )

  if (length(exports)) {
    msg <- c(
      msg,
      "",
      "See exports for more help:",
      paste0("  ?", if (is.null(pkg)) "", paste0(pkg, "::", exports))
    )
  }

  msg
}

#' @export
`as.character.cnd::condition_function` <- function(x, ...) {
  cnd(cond_as_character_condition())
}

#' @export
`all.equal.cnd::condition_function` <- function(target, current, ...) {
  op <- options(useFancyQuotes = FALSE)
  on.exit(options(op))

  mode_check <- all.equal(mode(target), mode(current))
  if (!isTRUE(mode_check)) {
    return(mode_check)
  }

  new <- as.list(environment(current))
  old <- as.list(environment(target))

  if (isTRUE(all.equal(new, old))) {
    return(TRUE)
  }

  bad <- character()

  # TODO relying on the default all.equal() checks.  I'd like
  # something a little more sophisticated and detailed
  for (field in names(new)) {
    check <- all.equal(new[[field]], old[[field]])
    if (isTRUE(check)) {
      next
    }

    bad <- c(bad, check)
  }

  bad
}

# conditions --------------------------------------------------------------

cond_no_package_exports <- function() {}
delayedAssign(
  "cond_no_package_exports",
  condition(
    "no_package_exports",
    type = "warning",
    message = "No package was supplied, so `exports` is ignored",
    exports = "condition",
    package = "cnd",
    help = "The `exports` parameter requires a `package`"
  )
)


cond_condition_bad_message <- function() {}
delayedAssign(
  "cond_condition_bad_message",
  condition(
    "invalid_condition_message",
    type = "error",
    message = "`message` must be a character vector or a function.",
    exports = "condition",
    package = "cnd",
    help = "
    Conditions messages are displayed when invoked through conditionMessage().
    You can set a static message by passing through a `character` vector, or a
    dynamic message by passing through a `function`.  The function should return
    a `character` vector.

    When `message` is not set, a default \"there was an error\" message is used.
    "
  )
)

cond_cnd_class <- function() {}
delayedAssign(
  "cond_cnd_class",
  condition(
    "cond_cnd_class",
    type = "error",
    message = "'condition' must be a `cnd::condition` object",
    exports = "cnd",
    package = "cnd",
    help = "
    `cnd()` simple calls the appropriate function: `stop()`, `warning()`, or
    `message()` based on the `type` parameter from `cnd::condition()`
    "
  )
)


cond_as_character_condition <- function() {}
delayedAssign(
  "cond_as_character_condition",
  condition(
    "as_character_cnd_error",
    type = "error",
    package = "cnd",
    message = c(
      "You are trying to coerce a `cnd::condition_function` object to a ",
      "character.\nDid you mean instead to call it as a function first?"
    ),
    help = c(
      "You cannot coerce a `cnd::condition_function` object to a character. ",
      "This may have occured when trying to put a condition function through ",
      "`stop()` or `warning`.  Instead, call the function first, then pass the",
      " result to `stop()` or `warning()`.",
      "
\nFor example:

```r
# Instead of this
stop(my_condition)

# Do this
stop(my_condition())
```
"
    )
  )
)

cond_condition_invalid <- NULL
delayedAssign(
  "cond_condition_invalid",
  condition(
    "invalid_condition",
    type = "error",
    message = function(problems) {
      collapse(
        "The following problems were found with the condition:",
        paste0("\n", problems)
      )
    },
    package = "cnd",
    exports = "condition",
    help = "
    The `class`, `exports`, and `help` parameters must be a single character
    string.  If you are passing a function, it must be a valid function.
    "
  )
)
