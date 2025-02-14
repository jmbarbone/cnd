
# register ----------------------------------------------------------------

#' Register a condition
#'
#' Only register functions that are associated with a package
#'
#' @param cond A condition object
#' @param old The old condition
#' @param registry A registry name
#'
#' @noRd
register_condition <- function(cond, old = NULL, registry = NULL) {
  # See if there's already a condition created, just based on name and package.
  # The other values may change, which will be noted in the overwrite
  if (is.null(old)) {
    old <- do_find_cond(cond, force = TRUE, check = c("class", "package"))
    if (!is.null(old)) {
      old <- old[[1L]]
    }
  }

  # could play around with identical()
  if (isTRUE(all.equal(cond, old))) {
    return(invisible())
  }

  if (!is.null(old)) {
    cnd(cond_condition_overwrite(old, cond))
  }

  if (is.null(registry)) {
    pkg <- cget(cond, "package")
    if (is.null(pkg)) {
      return()
    }
    registry <- get_registry(pkg)
  } else {
    registry <- get_registry(registry)
  }

  assign(cget(cond, "class"), cond, registry)
  invisible()
}


# global registry ---------------------------------------------------------

global_registry <- new.env(hash = FALSE)
class(global_registry) <- c("cnd:registry", "environment")
attr(global_registry, "global") <- TRUE

local(envir = global_registry, {
  .self <- global_registry

  new_registry <- function() {
    e <- new.env(parent = .self, hash = FALSE)
    class(e) <- c("cnd:registry", "environment")
    e
  }

  create_registry <- function(x) {
    e <- assign(x, .self$new_registry(), .self$registries)
    assign(".__NAME__.", x, e)
    e
  }

  remove_registry <- function(x) {
    if (inherits(x, "cnd:registry")) {
      x <- get(".__NAME__.", x, inherits = TRUE)
    }

    if (exists(x, .self$registries, inherits = FALSE)) {
      rm(list = x, envir = .self$registries)
    }
  }

  get_registry <- function(x) {
    if (inherits(x, "cnd:registry")) {
      return(x)
    }

    if (!exists(x, .self$registries, inherits = FALSE)) {
      return(.self$create_registry(x))
    }

    get(x, .self$registries, inherits = FALSE)
  }

  # a new environment for each registry
  registries <- new_registry()
  class(registries) <- c("cnd:registry", "environment")
  attr(registries, "list") <- TRUE
})


# helpers -----------------------------------------------------------------

create_registry <- function(name) {
  global_registry$create_registry(name)
}

get_registry <- function(registry) {
  global_registry$get_registry(registry)
}

remove_registry <- function(registry) {
  global_registry$remove_registry(registry)
}

unregister_condition <- function(cond, registry = cond$package) {
  cond <- find_cond(cond)
  force(registry)
  rm(list = cond$class, envir = get_registry(registry))
  invisible()
}


# methods -----------------------------------------------------------------

#' @export
`print.cnd:registry` <- function(x, ...) {
  nm <- get0(".__NAME__.", x, inherits = TRUE)
  cat(
    if (isTRUE(attr(x, "global"))) "global ",
    "registry",
    if (isTRUE(attr(x, "list"))) " list",
    if (!is.null(nm)) paste0(" '", nm, "'"),
    paste0("\n  ", ls(x, sorted = TRUE)),
    sep = ""
  )
  invisible(x)
}

as_list_env <- function(x, ...) {
  as.list.environment(x, all.names = FALSE, sorted = TRUE)
}

#' @export
`as.list.cnd:registry` <- as_list_env



# conditions --------------------------------------------------------------

cond_condition_overwrite <- NULL
delayedAssign(
  "cond_condition_overwrite",
  condition(
    "condition_overwrite",
    type = "warning",
    package = "cnd",
    # nolint next: brace_linter.
    message = \(old, new) fmt(
      "A condition with the class name '{cls}' already exists in '{pkg}' and",
      " will be overwritten{diff}",
      cls = old$class,
      pkg = old$package,
      diff = paste0("\n   ", all.equal(old, new), collapse = "")
    )
  )
)
