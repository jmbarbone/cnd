#' Document your conditions
#'
#' Documents your [conditions()] and [cnd::conditions()]
#'
#' @param package The package to document
#' @param registry The name of the registry
#' @param path The path to save the documentation
#'
#' @export
#' @returns A `character` vector`
cnd_document <- function(
    package = get_package(),
    registry = package,
    path = file.path("R", paste0(package, "-cnd-conditions.R"))
) {
  force(package)
  force(path)
  conds <- conditions(package = package, registry = registry)

  res <- fmt(
    sub("[@]noRd", "@export", cnd_documentation_fmt),
    package = package,
    # nolint start: line_length_linter.
    aliases1 = collapse(vapply(conds, cget, NA_character_, "class"), sep = " "),
    aliases2 = collapse(vapply(conds, cget, NA_character_, ".class"), sep = " "),
    aliases3 = collapse(vapply(conds, `format.cnd::condition_generator`, NA_character_), sep = " "),
    # nolint end: line_length_linter.
    cnd_section_describe = collapse(
      vapply(
        conds,
        function(c) {
          fmt(
            sub("@keywords internal", "", cnd_section_describe_fmt),
            form = format(c),
            pkg = cget(c, "package"),
            cls = cget(c, "class"),
            typ = cget(c, "type"),
            help = if (is.null(h <- cget(c, "help"))) {
              "#'"
            } else {
              paste0("#'   ", clean_text(h), collapse = "\n")
            }
          )
        },
        NA_character_
      )
    )
  )
  cat(res, file = path, sep = "")
}

cnd_documentation_fmt <- "
#' @name {package}-conditions
#' @aliases {package}-conditions {aliases1} {aliases2} {aliases3}
#' @title Conditions for `{package}`
#'
#' @details
#'  The following conditions are defined in the `{{package}}` package.
#'
#' @section [`{cnd}`][cnd::cnd-package]:
#'  These conditions are made with the `{cnd}` package though the use of
#'  [cnd::condition()].
#'
#' @section `{{package}}` conditions:
#' {cnd_section_describe}
#'
#' @seealso [cnd::cnd-package] [cnd::condition]
#' @noRd
#' @keywords internal
'_PACKAGE'
"

cnd_section_describe_fmt <- "
#' @keywords internal
#'   \\subsection{`{form}`}{
#'   \\describe{
#'     \\item{package}{`{{pkg}}`}
#'     \\item{class}{`{cls}`}
#'     \\item{type}{**{typ}**}
#'   }
{help}
#'   }
#'"

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
Conditions are generated through the [`{cnd}`][cnd::cnd-package] package.
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
