
#' @name cnd-conditions
#' @aliases cnd-conditions cnd:as_character_cnd_error cnd:cond_cnd_class cnd:condition_overwrite cnd:invalid_condition cnd:invalid_condition_message cnd:match_arg cnd:no_package_exports as_character_cnd_error cond_cnd_class condition_overwrite invalid_condition invalid_condition_message match_arg no_package_exports cnd:as_character_cnd_error/error cnd:cond_cnd_class/error cnd:condition_overwrite/warning cnd:invalid_condition/error cnd:invalid_condition_message/error cnd:match_arg/error cnd:no_package_exports/warning
#' @title Conditions for `cnd`
#'
#' @details
#'  The following conditions are defined in the `{cnd}` package.
#'
#' @section [`{cnd}`][cnd::cnd-package]:
#'  These conditions are made with the `{cnd}` package though the use of
#'  [cnd::condition()].
#'
#' @section `{cnd}` conditions:
#' 
#' 
#'   \subsection{`cnd:as_character_cnd_error/error`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:as_character_cnd_error`}
#'     \item{type}{**error**}
#'   }
#'   You cannot coerce a [cnd::condition_generator] object to a character. This may have occured when trying to put a condition function through [stop()] or [warning].  Instead, call the function first, then pass the result to [stop()] or [warning()].
#'   
#'   For example:
#'   
#'   ```r
#'   # Instead of this
#'   stop(my_condition)
#'   
#'   # Do this
#'   stop(my_condition())
#'   ```
#'   }
#'
#' 
#'   \subsection{`cnd:cond_cnd_class/error`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:cond_cnd_class`}
#'     \item{type}{**error**}
#'   }
#'   [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
#'   }
#'
#' 
#'   \subsection{`cnd:condition_overwrite/warning`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:condition_overwrite`}
#'     \item{type}{**warning**}
#'   }
#'
#'   }
#'
#' 
#'   \subsection{`cnd:invalid_condition/error`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:invalid_condition`}
#'     \item{type}{**error**}
#'   }
#'   The `class`, `exports`, and `help` parameters must be a single character string.  If you are passing a function, it must be a valid function.
#'   }
#'
#' 
#'   \subsection{`cnd:invalid_condition_message/error`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:invalid_condition_message`}
#'     \item{type}{**error**}
#'   }
#'   Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
#'   
#'   When `message` is not set, a default "there was an error" message is used.
#'   }
#'
#' 
#'   \subsection{`cnd:match_arg/error`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:match_arg`}
#'     \item{type}{**error**}
#'   }
#'   Mostly [match.arg()] but with a custom condition
#'   }
#'
#' 
#'   \subsection{`cnd:no_package_exports/warning`}{
#'   \describe{
#'     \item{package}{`{cnd}`}
#'     \item{class}{`cnd:no_package_exports`}
#'     \item{type}{**warning**}
#'   }
#'   The `exports` parameter requires a `package`
#'   }
#'
#'
#' @seealso [cnd::cnd-package] [cnd::condition]
#' @export
#' @keywords internal
'_PACKAGE'
