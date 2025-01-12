#' Conditions
#'
#' Conditions
#'
#' @description
#' [conditions()] is used to create a new condition function that itself returns
#' a new `condition`.
#' [conditions()] retrieves all conditions based on search values
#'
#' @param class The name of the new class
#' @param message The message to be displayed when the condition is called
#' @param type The type of condition: error, warning, or message
#' @param exports The exported functions to be displayed when the condition is
#'   called
#' @param help The help message to be displayed for the condition function
#' @param package The package to which the condition belongs
#'
#' @returns
#' - [condition()] a `cnd::condition_function` object
#' - [conditions()] a `list` of all conditions
#' @export
#' @examples
#' # create a new condition:
#' cond_value_error <- condition("value_error")
#'
#' # use the condition
#' try(stop(cond_value_error()))
#'
#' # dynamic messages:
#' cond_class_error <- condition(
#'   "class_error",
#'   message = \(x) paste("class cannot be", toString(class(x)))
#' )
#' try(stop(cond_class_error(list())))
#'
#' @include utils.R
condition <- function(
    class,
    message = NULL,
    type = c("error", "warning", "message"),
    exports = NULL,
    help = NULL,
    package = get_package()
) {
  force(package)

  # TODO add custom condition
  stopifnot(
    is.character(class),
    !grepl("::", class, fixed = TRUE),
    length(class) == 1L,
    is.null(exports) || is.character(exports),
    is.null(help) || is.character(help)
  )

  type <- match.arg(type)

  # if (!endsWith(class, type)) {
  #   # appending another class to the condition so we get explicit
  #   # "condition_error"
  #   class <- c(paste(class, type, sep = "_"), class)
  # }

  if (!is.null(package)) {
    class <- paste(package, class, sep = "::")
  }

  if (is.null(message)) {
    message <- function() "\b there was an error"
  } else if (is.character(message)) {
    message <- as.function(list(paste0(message, collapse = "")))
  } else if (!is.function(message)) {
    stop(condition_message_error())
  }

  # setting up an environment to track additional fields for
  condition_env <- new.env(parent = capsule)
  assign("..", condition_env, condition_env)
  assign("message", message, condition_env)
  assign("exports", exports, condition_env)
  assign("package", package, condition_env)
  assign("class", class, condition_env)
  assign("type", type, condition_env)
  assign("help", help, condition_env)

  res <- local(envir = condition_env, {
    function() {
      params <- as.list(match.call())[-1L]
      # TODO add `call` to the formals
      cond <- list(message = do.call(..$message, params), call = NULL)
      base::class(cond) <- c(..$class, "cnd::condition", ..$type, "condition")
      attr(cond, "help") <- ..$help
      attr(cond, "package") <- ..$package
      attr(cond, "exports") <- ..$exports
      attr(cond, "condition") <- ..$class
      cond
    }
  })

  formals(res) <- formals(message)
  base::class(res) <- c("cnd::condition_function", "function")

  if (
    isTRUE(getOption("cnd.verbose", TRUE)) &&
    exists(class[1L], envir = .__conditions__., inherits = FALSE)
  ) {
    warning(condition_condition_warning(class[1L]))
  }

  assign(class[1L], res, envir = .__conditions__.)
  res
}

class(condition) <- c("cnd::condition_generator", "function")

#' @export
#' @rdname condition
conditions <- function(class = NULL, package = NULL) {
  # TODO search on names only could reduce this to a smaller object
  conds <- as.list(.__conditions__.)

  if (!is.null(class)) {
    conds <- filter2(conds, \(x) sub("^.*::", "", x$condition) == condition)
  }

  if (!is.null(package)) {
    conds <- filter2(conds, \(x) x$package == package)
  }

  conds
}

# conditions --------------------------------------------------------------

get_condition_error <- condition(
  "get_condition_error",
  type = "error",
  message = function(condition)
    sprintf(
      c(
        "Condition '%s' does not exist.  Create a new condition with",
        " `cnd::condition()`."
      ),
      condition
    ),
  exports = "condition",
  package = "cnd"
)

condition_message_error <- condition(
  "condition_message_error",
  type = "error",
  message = "`message` must be a character vector or a function.",
  exports = "condition",
  package = "cnd"
)

condition_condition_warning <- condition(
  "condition_warning",
  type = "warning",
  message = function(class)
    sprintf("Condition '%s' already exists. Overwriting.", class),
  exports = "condition",
  package = "cnd"
)
