suppress_conditions <- function(expr, classes = "condition") {
  withCallingHandlers(
    expr,
    condition = function(c) {
      if (inherits(c, classes)) {
        tryInvokeRestart("muffleCondition")
      }
    }
  )
}

# nolint next: object_name_linter.
suppressConditions <- suppress_conditions

suppress_cnd_conditions <- function(expr) {
  suppress_conditions(expr, "cnd::condition")
}

cnd_condition <- function(condition, fmt = c("verbse", "simple")) {
  cnd_message_handler(condition, fmt, "condition")
}

cnd_message <- function(condition, fmt = c("verbose", "simple")) {
  cnd_message_handler(condition, fmt, "message")
}

cnd_message_handler <- function(
  condition,
  fmt = c("verbose", "simple"),
  type = c("message", "condition")
) {
  fmt <- match_arg(fmt, .null_as_default = TRUE)
  type <- match_arg(type)

  with_restarts <- function(output, fmt, ...) {
    # stderr() for messages, stdout() for condition
    handler <- function(x, fmt, output) {
      msg <- switch(fmt, simple = conditionMessage(x), verbose = format(x))
      cat(msg, sep = "\n", file = output)
    }

    withRestarts(
      {
        signalCondition(condition)
        handler(condition, fmt, output)
      },
      ...
    )
  }

  switch(
    type,
    message = with_restarts(stderr(), fmt, muffleMessage = function() NULL),
    condition = with_restarts(stdout(), fmt, muffleCondition = function() NULL)
  )
}
