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

is_condition <- function(x) {
  inherits(x, "condition")
}

is_cnd_condition <- function(x) {
  inherits(x, "cnd_condition")
}

is_cnd_generator <- function(x) {
  inherits(x, "cnd_condition_generator")
}

is_cnd_function <- function(x) {
  inherits(x, "cnd_condition_function")
}

is_error_cnd <- function(x) {
  is_cnd_function(x) && attr(x, "type") == "error"
}

is_warning_cnd <- function(x) {
  is_cnd_function(x) && attr(x, "type") == "warning"
}

is_message_cnd <- function(x) {
  is_cnd_function(x) && attr(x, "type") == "message"
}
