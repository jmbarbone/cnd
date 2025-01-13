#' Document your conditions
#'
#' Documents your [conditions()] and [cnd::conditions()]
#'
#' @param package The package to document
#'
#' @export
#' @returns Nothing, called for its side-effects
cnd_document <- function(package = get_package()) {
  stopifnot(!is.null(package))
  conds <- conditions(package)
  rd <- fmt(
    cnd_document_fmt,
    aliases1 = collapse(
      paste0("\n\\alias{", vapply(conds, \(c) c$class, NA_character_), "}")
    ),
    aliases2 = collapse(
      paste0("\n\\alias{", vapply(conds, format, NA_character_), "}")
    ),
    package = package,
    cnd1 = if (package == "cnd") "=conditions" else "cnd:conditions",
    cnd2 = if (package == "cnd") "conditions" else "cnd::conditions",
    conds_docs = collapse(vapply(conds, cond_to_doc, NA_character_))
  )
  rd <- clean_text(rd)
  writeLines(rd, fmt("man/{pkg}_cnd_conditions.Rd", pkg = package))
  invisible()
}

cond_to_doc <- function(condition) {
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
        paste0("\n\nsee: ", to_string(fmt(
          "\\code{\\link[{pkg1}{exp}]{{exp}{fun}}}",
          pkg1 = if (condition$package == "cnd") "=" else "cnd:",
          pkg2 = if (condition$package == "cnd") "" else "cnd::",
          exp = condition$exports,
          fun = if (
            is.function(get(
              condition$exports,
              parent.env(get_registry(condition$package))
            ))
          ) {
            "()"
          } else {
            ""
          })
        ))
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
{conds_docs}"

cond_to_doc_fmt <- "
\\section{\\code{{clsfmt}}}{
\\describe{
  \\item{class}{{cls}}
  \\item{type}{{typ}}
  \\item{package}{{pkg}}
}{help}{exports}
}"


# TODO consider different formatting for the help section
"warning/cnd:::cond_no_package_exports"
"cnd[warning/cond_no_package_exports]"
"cnd:::cond_no_package_exports[warning]"
"cnd:::cond_no_package_exports/warning"
"condition[warning/condition_warning]"
"cnd:::condition[warning/condition_warning]"
