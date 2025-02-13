% Generated with cnd::cnd_document("cnd")
\name{cnd-conditions}
\alias{cnd-conditions}
\alias{cnd:no_package_exports}
\alias{cnd:match_arg}
\alias{cnd:as_character_cnd_error}
\alias{cnd:cond_cnd_class}
\alias{cnd:invalid_condition_message}
\alias{cnd:condition_overwrite}
\alias{cnd:invalid_condition}
\title{Conditions for \code{{cnd}}}
\description{
  The following conditions are defined in the cnd package.
  For more information on \code{conditions} see
  \code{\link[=conditions]{conditions()}}
}

\section{\code{cnd:no_package_exports}}{
\describe{
  \item{class}{cnd:no_package_exports}
  \item{type}{warning}
  \item{package}{cnd}
}
The `exports` parameter requires a `package`

see: \code{\link[=condition]{condition()}}
}
\section{\code{cnd:match_arg}}{
\describe{
  \item{class}{cnd:match_arg}
  \item{type}{error}
  \item{package}{cnd}
}
}
\section{\code{cnd:as_character_cnd_error}}{
\describe{
  \item{class}{cnd:as_character_cnd_error}
  \item{type}{error}
  \item{package}{cnd}
}
You cannot coerce a `cnd::condition_function` object to a character. This may have occured when trying to put a condition function through `stop()` or `warning`.  Instead, call the function first, then pass the result to `stop()` or `warning()`.

For example:

```r
# Instead of this
stop(my_condition)

# Do this
stop(my_condition())
```
}
\section{\code{cnd:cond_cnd_class}}{
\describe{
  \item{class}{cnd:cond_cnd_class}
  \item{type}{error}
  \item{package}{cnd}
}
`cnd()` simple calls the appropriate function: `stop()`, `warning()`, or
`message()` based on the `type` parameter from `cnd::condition()`

see: \code{\link[=cnd]{cnd()}}
}
\section{\code{cnd:invalid_condition_message}}{
\describe{
  \item{class}{cnd:invalid_condition_message}
  \item{type}{error}
  \item{package}{cnd}
}
Conditions messages are displayed when invoked through conditionMessage().
You can set a static message by passing through a `character` vector, or a
dynamic message by passing through a `function`.  The function should return
a `character` vector.

When `message` is not set, a default "there was an error" message is used.

see: \code{\link[=condition]{condition()}}
}
\section{\code{cnd:condition_overwrite}}{
\describe{
  \item{class}{cnd:condition_overwrite}
  \item{type}{warning}
  \item{package}{cnd}
}
}
\section{\code{cnd:invalid_condition}}{
\describe{
  \item{class}{cnd:invalid_condition}
  \item{type}{error}
  \item{package}{cnd}
}
The `class`, `exports`, and `help` parameters must be a single character
string.  If you are passing a function, it must be a valid function.

see: \code{\link[=condition]{condition()}}
}
