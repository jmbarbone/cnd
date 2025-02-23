% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document.R
\name{cnd_document}
\alias{cnd_document}
\alias{cnd_section}
\title{Document your conditions}
\usage{
cnd_document(
  package = get_package(),
  registry = package,
  file = file.path("R", paste0(package, "-cnd-conditions.R")),
  cleanup = TRUE
)

cnd_section(fun)
}
\arguments{
\item{package}{The package to document}

\item{registry}{The name of the registry}

\item{file}{The file to save the documentation.  This can be a file path, a
connection object, or \code{NULL}.  When \code{file} is a path, the directory of the
path is searched for files containing \verb{# \% Generated by cnd: do not edit by hand}. These are removed if they are not the same as the generated
documentation.}

\item{cleanup}{If \code{FALSE} will not remove files containing \verb{# \% Generated by cnd: do not edit by hand}}

\item{fun}{The name of a function}
}
\value{
\itemize{
\item \code{\link[=cnd_document]{cnd_document()}} Conditional on the \code{file} argument:
\itemize{
\item when \code{file} is a connection, the connection object
\item when \code{file} is a path, the path
\item when \code{file} is \code{NULL}, a \code{character} vector of the documentation
\item if no conditions are found, a warning is thrown and \code{NULL} is returned
}
}

\itemize{
\item \code{\link[=cnd_section]{cnd_section()}} A \code{character} vector of the documentation
}
}
\description{
Documents your \code{\link[=conditions]{conditions()}} and \code{\link[=conditions]{conditions()}}
}
\section{conditions}{


Conditions are generated through the \code{\link[=cnd-package]{\{cnd\}}} package.
The following conditions are associated with this function:

\describe{

\item{\code{\link[=cnd-cnd-conditions]{cnd:cnd_document_conditions/warning}}}{

}

\item{\code{\link[=cnd-cnd-conditions]{cnd:cnd_document_file/error}}}{

}

\item{\code{\link[=cnd-cnd-conditions]{cnd:cnd_document_pkg_reg/error}}}{

}

\item{\code{\link[=cnd-cnd-conditions]{cnd:cnd_generated_cleanup/message}}}{

}

\item{\code{\link[=cnd-cnd-conditions]{cnd:cnd_generated_write/condition}}}{

}

}

For more conditions, see: \link{cnd-cnd-conditions}
}

\examples{
file <- file()
cnd_document("cnd", file = file)
readLines(file)

cnd_section("cnd")
}
