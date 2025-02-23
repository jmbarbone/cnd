% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{cnd_is}
\alias{cnd_is}
\alias{is_condition}
\alias{is_cnd_condition}
\alias{is_cnd_generator}
\alias{is_conditioned_function}
\title{\code{is} functions for \link{cnd}}
\usage{
is_condition(x)

is_cnd_condition(x)

is_cnd_generator(x, type = c("error", "warning", "message", "condition"))

is_conditioned_function(x)
}
\arguments{
\item{x}{An object}

\item{type}{A specific type to check}
}
\value{
\code{TRUE} or \code{FALSE} for the test
}
\description{
\code{is} functions for \link{cnd}
}
\examples{
is_condition(simpleCondition(""))
is_cnd_condition(simpleCondition(""))

con <- condition("is")
is_condition(con)
is_cnd_condition(con)

is_condition(con())
is_cnd_condition(con())

is_cnd_generator(con)

is_conditioned_function(cnd)
}
