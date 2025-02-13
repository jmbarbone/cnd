#' Document your conditions
#'
#' Documents your [conditions()] and [cnd::conditions()]
#'
#' @param package The package to document
#' @param env The environment to attach the package name
#'
#' @export
#' @returns Nothing, called for its side-effects
cnd_document <- function(package = get_package(env), env = parent.frame()) {
  force(package)
  conds <- conditions(package = package)

  rd <- fmt(
    cnd_document_fmt,
    aliases1 = collapse(
      paste0("\n\\alias{", vapply(conds, \(c) c$class, NA_character_), "}")
    ),
    package = package,
    cnd1 = if (package == "cnd") "=conditions" else "cnd:conditions",
    cnd2 = if (package == "cnd") "conditions" else "cnd::conditions",
    conds_docs = collapse(vapply(conds, cond_to_doc, NA_character_, env = env))
  )

  rd <- clean_text(rd)
  writeLines(rd, fmt("man/{pkg}_cnd_conditions.Rd", pkg = package))
  invisible()
}

cond_to_doc <- function(condition, env) {
  stopifnot(is_cnd_function(condition))
  fmt(
    cond_to_doc_fmt,
    clsfmt = fmt(condition$class),
    cls = condition$class,
    pkg = condition$package,
    typ = condition$type,
    help =
      if (is.null(condition$help)) {
        ""
      } else {
        paste0("\n", collapse(clean_text(condition$help), sep = "\n"))
      },
    exports =
      if (is.null(condition$exports)) {
        ""
      } else {
        paste0(
          "\n\nsee: ",
          to_string(
            fmt(
              "\\code{\\link[{pkg1}{exp}]{{exp}{fun}}}",
              pkg1 = if (condition$package == "cnd") "=" else "cnd:",
              pkg2 = if (condition$package == "cnd") "" else "cnd::",
              exp = condition$exports,
              # not the same as is(_, "function") or inherits(_, "function")
              fun = if (is.function(get(condition$exports, env))) "()" else ""
            )
          )
        )
      }
  )
}

cnd_document_fmt <- "% Generated with cnd::cnd_document(\"{package}\")
\\name{{package}-conditions}
\\alias{{package}-conditions}{aliases1}{aliases2}
\\title{Conditions for \\code{{{package}}}}
\\description{
  The following conditions are defined in the {package} package.
  For more information on \\code{conditions} see
  \\code{\\link[{cnd1}]{{cnd2}()}}
}
\\section{\\pkg{cnd}}{
  \\code{\\link[cnd:cnd-package]{cnd}} is the package that provides this stuff
}
{conds_docs}
"

cond_to_doc_fmt <- "
  \\section{\\code{{clsfmt}}}{
  \\describe{
    \\item{class}{{cls}}
    \\item{type}{{typ}}
    \\item{package}{{pkg}}
  }{help}{exports}
  }
"

cnd_document_fmt <- sub("{aliases2}", "", cnd_document_fmt, fixed = TRUE)


#' @export
#' @rdname cnd_document
#' @param fun The name of a function
cnd_section <- function(fun) {
  conds <- conditions(fun)
  fmt(
    cnd_section_fmt,
    conds = collapse(
      vapply(
        conds,
        function(c) {
          fmt(
            cnd_section_item_fmt,
            pkg = cget(c, "package"),
            form = format(c),
            help = cget(c, "help") %||% ""
          )
        },
        NA_character_
      )
    ),
    pkgs = collapse(
      vapply(
        unique(vapply(conds, cget, NA_character_, "package")),
        function(p) fmt(cnd_section_seealso_fmt, pkg = p),
        NA_character_
      )
    ),
    sep = ", "
  )
}


cnd_section_fmt <- "
Conditions are generated through the \\code{\\link[cnd-package]{cnd}} package.
The following conditions are associated with this function:

\\describe{
  {conds}
}

For more conditions, see: {pkgs}
"

cnd_section_item_fmt <- "
  \\item{\\code{\\link[{pkg}:{pkg}-conditions]{{form}}}}{
    {help}
  }
"

cnd_section_seealso_fmt <- "
\\code{\\link{{pkg}-conditions}}
"
