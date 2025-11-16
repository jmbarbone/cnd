# register ----------------------------------------------------------------

#' Create a registration
#'
#' Crate a new `cnd:registry` to the current environment
#'
#' @description This function will create a new object with the name as `name`
#' in the environment where it is called.  This is intended to be your package
#' environment, but could potentially be anywhere you want.  If an object which
#' is not a `cnd:registry` object is found with the same name, an error will be
#' thrown.
#'
#' @param registry The name of the registry
#' @param overwrite When `TRUE` will overwrite
#' @param name The name of the registry variable.  Default is intended to
#'   prevent potential conflicts with other objects.
#' @param env The environment to assign the registry to
#' @examples
#' # In most cases, just having the function in your R/ scripts is good enough,
#' # and you can use `cnd_create_registry()` with its defaults.  The following
#' # examples are for demonstration purposes:
#' e <- new.env()
#' cnd_create_registry("EXAMPLE", env = e)
#' cnd_create_registry("EXAMPLE", overwrite = TRUE)
#'
#' @returns a `cnd:registry` object, invisibly
#' @export
cnd_create_registry <- function(
  registry = get_package(),
  overwrite = FALSE,
  name = ".__CND_REGISTRY__.",
  env = parent.frame()
) {
  registrar$create(
    registry = registry,
    overwrite = overwrite,
    name = name,
    env = env
  )
}

# registrar ---------------------------------------------------------------

registrar <- new.env()
class(registrar) <- "cnd:registrar"
.cnd_env <- environment()

local(envir = registrar, {
  .self <- registrar

  new <- function(x = NULL) {
    e <- new.env(parent = .cnd_env)
    assign(".__NAME__.", x, e)
    class(e) <- "cnd:registry"
    e
  }

  add <- function(registry) {
    if (is.character(registry) && length(registry) == 1) {
      registry <- .self$new(registry)
    }

    if (!inherits(registry, "cnd:registry")) {
      # internal error
      stop("registry must be a 'cnd:registry' object") # nocov
    }

    assign(
      base::get(".__NAME__.", registry, inherits = FALSE),
      registry,
      .__REGISTRIES__.
    )
  }

  create <- function(registry, overwrite, name, env) {
    found <- get0(name, env, inherits = FALSE)
    if (!is.null(found)) {
      if (!inherits(found, "cnd:registry")) {
        stop(
          "You have a variable named '",
          name,
          "' in your environment,",
          " which is not a 'cnd:registry' object.  Please remove this from",
          " your package environment to create a registry for the cnd pacakge",
          call. = FALSE
        )
      }

      if (!overwrite) {
        return(invisible(found))
      }
    }

    # create the registration and assign it to both environments
    reg <- .self$add(registry)
    .self$add(reg)
    assign(name, reg, env)
  }

  remove <- function(x) {
    if (inherits(x, "cnd:registry")) {
      x <- base::get(".__NAME__.", x, inherits = TRUE)
    }

    if (exists(x, .__REGISTRIES__., inherits = FALSE)) {
      base::remove(list = x, envir = .__REGISTRIES__., inherits = TRUE)
    }
  }

  get <- function(x) {
    if (inherits(x, "cnd:registry")) {
      return(x)
    }

    force(.__CND_PACKAGE_REGISTRY__.)
    # See above for why base::get() isn't used.  Maybe if there is a more
    # sophisticated implementation of registry finding, then we can safely use
    # `get()` an allow errors to be thrown when objects do not exist.
    get0(x, .__REGISTRIES__., mode = "environment")
  }

  register <- function(condition, old = NULL, registry = NULL) {
    force(condition)

    # See if there's already a condition created, just based on name and
    # package. The other values may change, which will be noted in the overwrite
    if (is.null(old)) {
      old <- do_find_cond(
        x = condition,
        force = TRUE,
        check = c("class", "package")
      )
      if (length(old)) {
        old <- old[[1L]]
      }
    }

    # could play around with identical()
    if (isTRUE(all.equal(condition, old))) {
      return(invisible())
    }

    if (!is.null(old)) {
      cnd(cond_condition_overwrite(old, condition))
    }

    if (is.null(registry)) {
      pkg <- cget(condition, "package")
      if (is.null(pkg)) {
        return()
      }
      registry <- .self$get(pkg)
    } else {
      registry <- .self$get(registry)
    }

    # TODO consider throwing a warning if the registry is null.  It will be null
    # if we haven't create a package registry environment yet, or if we are
    # just assigning a value to `package` for an example.  Maybe we include a
    # parameter to catch `missing(registry)` which, when `TRUE` will not throw
    # the warning.  But when `missing(registry)` is false, then there was an
    # explicit attempt to use a registry that doesn't exist.
    #
    # For now, everything will get pushed into the default registry
    registry <- registry %||% .self$get(":default:")
    assign(cget(condition, "class"), condition, registry)
  }

  unregister <- function(condition, registry = cget(condition, "package")) {
    condition <- cond(condition)
    force(registry)
    base::rm(list = condition$class, envir = .self$get(registry))
  }

  list <- function() {
    .__REGISTRIES__.
  }

  check <- function(package) {
    if (exists(package, .__REGISTRIES__.)) {
      return()
    }

    if (!requireNamespace(package, quietly = TRUE)) {
      return()
    }

    ns <- asNamespace(package)

    reg <-
      get0(".__CND_REGISTRY__.", ns, inherits = FALSE) %||%
      Filter(
        function(i) inherits(i, "cnd:registry"),
        as.list(ns, all.names = TRUE)
      )

    if (is.list(reg)) {
      reg <- reg[[1L]]
    }

    if (length(reg)) {
      .self$add(reg)
    }
  }

  ls <- .self$list
  rm <- .self$remove
  # a new environment for each registry
  .__REGISTRIES__. <- .self$new() # nolint: object_name_linter.

  # using the same class because why not
  class(.__REGISTRIES__.) <- "cnd:registries" # nolint: object_name_linter.
  attr(.__REGISTRIES__., "list") <- TRUE # nolint: object_name_linter.

  # default registry for all conditions
  registrar$add(":default:")
})


# methods -----------------------------------------------------------------

print_cnd_registrar <- function(x, ...) {
  name <- get0(".__NAME__.", x, inherits = FALSE)
  cat(
    switch(
      class(x),
      "cnd:registrar" = "REGISTRAR",
      "cnd:registry" = "REGISTRY",
      "cnd:registries" = "REGISTRIES"
    ),
    "\n",
    if (!is.null(name)) paste0("  '", name, "'\n"),
    paste0("  ", ls(x, sorted = TRUE), "\n"),
    sep = ""
  )
  invisible(x)
}

#' @export
`print.cnd:registrar` <- print_cnd_registrar

#' @export
`print.cnd:registry` <- print_cnd_registrar

#' @export
`print.cnd:registries` <- print_cnd_registrar

as_list_env <- function(x, all = FALSE) {
  force(x)
  as.list.environment(
    as.environment(x),
    sorted = TRUE,
    all.names = all
  )
}

# conditions --------------------------------------------------------------

# fmt: skip
cond_condition_overwrite <- function() {}
delayedAssign(
  "cond_condition_overwrite",
  condition(
    "condition_overwrite",
    type = "warning",
    package = "cnd",
    exports = "condition",
    # nolint next: brace_linter.
    message = function(old, new)
      fmt(
        "A condition with the class name '{cls}' already exists in '{pkg}' and",
        " will be overwritten{diff}",
        cls = old$class,
        pkg = old$package,
        diff = paste0("\n   ", all.equal(old, new), collapse = "")
      ),
    help = c(
      "Defining a new condition with the same class and package as an existing",
      " condition will overwrite the previous definition.  It is recommended",
      " to either avoid this by fully defining your condition, or creating a",
      " new condition instead."
    )
  )
)
