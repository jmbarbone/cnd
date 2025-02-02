#' Conditions
#'
#' Conditions
#'
#' @description
#' [conditions()] is used to create a new condition function that itself returns
#' a new `condition`. If a package is supplied, `[conditions()]` will register
#' the condition in the `.__cnd::condition_registry__.` object with the package
#' namespace.  If the object does not exist, **it will be registered in your
#' package namespace**.
#'
#' [conditions()] retrieves all conditions based on search values
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
#'
#' @include utils.R
#' @include register.R
condition <- function(
    class,
    message = NULL,
    type = c("error", "warning", "message"),
    package = get_package(),
    exports = NULL,
    help = NULL,
    register = !is.null(package)
) {
  # when no arguments are passed, return the existing condition
  old <- conditions(package = package, class = class)[[1L]]

  if (
    missing(message) &&
    missing(type) &&
    missing(exports) &&
    missing(help) &&
    missing(register) &&
    (missing(package) || !is.null(package)) &&
    !is.null(old)
  ) {
    return(old)
  }

  force(package)
  force(register)

  # TODO add custom conditions
  stopifnot(
    is.character(class),
    # must only contain letters, numbers and underscores or period
    grepl("^[a-z0-9_.]+$", class, ignore.case = TRUE),
    length(class) == 1L,
    is.null(exports) || is.character(exports),
    is.null(help) || is.character(help)
  )

  # TODO use custom match.arg()
  type <- match.arg(type)

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
  assign("..", condition_env, condition_env)
  assign("message", message, condition_env)
  assign("exports", exports, condition_env)
  assign("package", package, condition_env)
  assign("class", class, condition_env)
  assign("type", type, condition_env)
  assign("help", help, condition_env)

  res <- encapsulate(env = condition_env, {
    function() {
      params <- as.list(match.call())[-1L]
      params <- lapply(params, eval.parent, 2L)
      # TODO add `call` to the formals
      cond <- list(message = do.call(..$message, params), call = NULL)
      cond$message <- clean_text(cond$message)
      base::class(cond) <- c(..$class, "cnd::condition", ..$type, "condition")
      attr(cond, "help") <- ..$help
      attr(cond, "package") <- ..$package
      attr(cond, "exports") <- ..$exports
      attr(cond, "condition") <- ..$class
      attr(cond, "type") <- ..$type
      cond
    }
  })

  formals(res) <- formals(message)
  base::class(res) <- c("cnd::condition_function", "function")

  if (register) {
    register_condition(res, old)
  }

  res
}

class(condition) <- c("cnd::condition_generator", "function")

cond <- function(class, package = NULL) {
  conditions(package)[[paste(package, class, sep = ":")]]
}

#' @export
#' @rdname condition
#' @param ... Input argument.  If a function is passed, then defaults to passing
#'   `..1` to `fun`; otherwise defaults to passing `..1` to `package`
#' @param class,type,package Filtering
#' @param fun if a function is passed, then retrieves the `"conditions"` attribute
conditions <- function(
    ...,
    class = NULL,
    type = NULL,
    package = NULL,
    fun = NULL
) {

  if (...length()) {
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

  if (!is.null(package)) {
    conds <- filter2(conds, \(cond) cond$package == package)
  }

  if (!is.null(class)) {
    conds <- filter2(conds, \(cond) sub("^.*:", "", cond$class) == class)
  }

  if (!is.null(type)) {
    conds <- filter2(conds, \(cond) cond$type == type)
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
  # TODO use get_condition(condition)
  if (!is_cnd_condition(condition)) {
    cnd(cond_cnd_class())
  }

  switch(
    attr(condition, "type"),
    error = stop(condition), # maybe `error()` should be the name
    warning = warning(condition),
    message = message(condition)
  )
}

get_condition <- function(x) {
  if (is_cnd_condition(x)) {
    return(x)
  }

  nm <- sub("^.*/", "", x)
  ns <- sub("::.+$", "", nm)
  conditions(ns)[[nm]]
}

#' @export
#' @rdname condition
#' @param ... Additional arguments passed to methods
#' @param value A `condition`
`conditions<-` <- function(x, ..., value) {
  UseMethod("cnd.Rprojnditions<-")
}

#' @export
#' @rdname condition
#' @param append If `TRUE`, adds to the list of `conditions`
`conditions<-.function` <- function(x, append = FALSE, ..., value) {
  if (is.null(value)) {
    attr(x, "conditions") <- NULL
    class(x) <- setdiff(class(x), "cnd::conditioned_function")
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


# conditions --------------------------------------------------------------

cond_no_package_exports <- NULL
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

cond_condition_bad_message <- NULL
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

cond_cnd_class <- NULL
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
