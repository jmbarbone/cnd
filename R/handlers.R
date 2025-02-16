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

cnd_message <- function(condition) {
  fmt <- match_fmt()
  handler <- function(x) cat(x, sep = "\n", file = stderr())
  withRestarts(
    {
      signalCondition(condition)
      cat(
        switch(
          fmt,
          simple = conditionMessage(condition),
          verbose = format(condition)
        ),
        sep = "\n",
        file = stderr()
      )
    },
    muffleMessage = function() NULL
  )
  invisible(condition)
}

cnd_condition <- function(condition) {
  fmt <- match_fmt()
  handler <- function(x) cat(x, sep = "\n", file = stdout())
  withRestarts(
    {
      signalCondition(condition)
      cat(
        switch(
          fmt,
          simple = handler(conditionMessage(condition)),
          verbose = handler(format(condition))
        ),
        sep = "\n",
        file = stdout()
      )
    },
    muffleCondition = function() NULL
  )
  invisible(condition)
}

match_fmt <- function(fmt = c("verbose", "simple")) {
  match_arg(fmt, .call = sys.call(2L))
}
