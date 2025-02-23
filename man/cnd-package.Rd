% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnd-package.R
\docType{package}
\name{cnd-package}
\alias{cnd-package}
\title{cnd: Create and Register Conditions}
\description{
An interface for creating new condition generators, save generators in registries, link to functions, and generate documentation for packages.
}
\section{Options}{

\describe{
\item{\code{cnd.cli.override}\cr\verb{["on"|"off"|"none"]}}{
Controls override for printing messages from \code{{cli}}.  When \code{"on"} or
\code{"off"}, messages will be overridden to that state.  When \code{"none"},
\code{{cli}} will be used to determine appropriate state.
}
\item{\code{cnd.condition.message}\cr\verb{["verbose"|"simple"]}}{
How messages should be printed from conditions.  \code{"verbose"} will
provide the message, information about the condition's class, and call
information.  \code{"simple"} will only provide the message.
}
\item{\code{cnd.call}\cr\verb{[TRUE|FALSE]}}{
Whether to print the call that generated the condition.  This is
embedded within the \code{conditionCall()} method.
}
}
}

\seealso{
\code{\link[=condition]{condition()}} \link{cnd-cnd-conditions}
}
\author{
\strong{Maintainer}: Jordan Mark Barbone \email{jmbarbone@gmail.com} (\href{https://orcid.org/0000-0001-9788-3628}{ORCID}) [copyright holder]

}
\keyword{internal}
