# snapshots

    Code
      cat(cnd_section(cnd))
    Output
      
      Conditions are generated through the [`{cnd}`][cnd::cnd-package] package.
      The following conditions are associated with this function:
      
      \describe{
        
        \item{[`cnd:cond_cnd_class/error`][cnd-cnd-conditions]}{
          [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
        }
      
      }
      
      For more conditions, see: [cnd-cnd-conditions]

---

    Code
      cnd_document("cnd", file = stdout())
    Output
      #' @name cnd-cnd-conditions
      #' @aliases cnd-cnd-conditions cnd:as_character_cnd_error cnd:class_error cnd:cnd_document_conditions cnd:cnd_document_file cnd:cnd_document_pkg_reg cnd:cnd_generated_cleanup cnd:cnd_generated_write cnd:cond_cnd_class cnd:condition_message_generator cnd:condition_overwrite cnd:conditions_dots cnd:defunct_error cnd:deprecation_warning cnd:input_error cnd:input_warning cnd:invalid_condition cnd:invalid_condition_message cnd:match_arg cnd:match_error cnd:namespace_error cnd:no_package_exports cnd:path_deletion_message cnd:path_overwrite_message cnd:type_error cnd:value_error cnd:value_warning as_character_cnd_error class_error cnd_document_conditions cnd_document_file cnd_document_pkg_reg cnd_generated_cleanup cnd_generated_write cond_cnd_class condition_message_generator condition_overwrite conditions_dots defunct_error deprecation_warning input_error input_warning invalid_condition invalid_condition_message match_arg match_error namespace_error no_package_exports path_deletion_message path_overwrite_message type_error value_error value_warning cnd:as_character_cnd_error/error cnd:class_error/error cnd:cnd_document_conditions/warning cnd:cnd_document_file/error cnd:cnd_document_pkg_reg/error cnd:cnd_generated_cleanup/message cnd:cnd_generated_write/condition cnd:cond_cnd_class/error cnd:condition_message_generator/error cnd:condition_overwrite/warning cnd:conditions_dots/warning cnd:defunct_error/error cnd:deprecation_warning/warning cnd:input_error/error cnd:input_warning/warning cnd:invalid_condition/error cnd:invalid_condition_message/error cnd:match_arg/error cnd:match_error/error cnd:namespace_error/error cnd:no_package_exports/warning cnd:path_deletion_message/message cnd:path_overwrite_message/message cnd:type_error/error cnd:value_error/error cnd:value_warning/warning
      #' @title Conditions for `cnd`
      #'
      #' @details
      #'   The following conditions are defined in the `{cnd}` package.
      #'
      #' @section [`{cnd}`][cnd-package]:
      #'   These conditions are made with the `{cnd}` package though the use of
      #'   [cnd::condition()].
      #'
      #' @section `{cnd}` conditions:
      #'
      #'   \subsection{`cnd:as_character_cnd_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:as_character_cnd_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   You cannot coerce a [cnd::condition_generator] object to a character. This may have occurred when trying to put a condition function through [stop()] or [warning].  Instead, call the function first, then pass the result to [stop()] or [warning()].
      #'
      #'   For example:
      #'   ```r
      #'   # Instead of this
      #'   stop(my_condition)
      #'
      #'   # Do this
      #'   stop(my_condition())
      #'   ```
      #'  }
      #'   \subsection{`cnd:class_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:class_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cnd_document_conditions/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cnd_document_conditions`}
      #'     \item{type}{**warning**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cnd_document_file/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cnd_document_file`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cnd_document_pkg_reg/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cnd_document_pkg_reg`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cnd_generated_cleanup/message`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cnd_generated_cleanup`}
      #'     \item{type}{**message**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cnd_generated_write/condition`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cnd_generated_write`}
      #'     \item{type}{**condition**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:cond_cnd_class/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:cond_cnd_class`}
      #'     \item{type}{**error**}
      #'   }
      #'   [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
      #'  }
      #'   \subsection{`cnd:condition_message_generator/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:condition_message_generator`}
      #'     \item{type}{**error**}
      #'   }
      #'   [cnd::condition_generator] objects are not conditions.   You may have made this mistake:
      #'
      #'   ```r
      #'   x <- condition("my_condition")
      #'   conditionMessage(x)
      #'   ```
      #'
      #'   Condition generators need to be called first before they can be used as conditions.  Try this instead:
      #'
      #'   ```r
      #'   x <- condition("my_condition")
      #'   conditionMessage(x())
      #'   ```
      #'  }
      #'   \subsection{`cnd:condition_overwrite/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:condition_overwrite`}
      #'     \item{type}{**warning**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:conditions_dots/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:conditions_dots`}
      #'     \item{type}{**warning**}
      #'   }
      #'   The `...` parameter in [conditions()] is meant for convenience.  Only a single argument is allowed.  Other parameters must be named  explicitly.
      #'
      #'   For example:
      #'
      #'   ```r
      #'   # Instead of this
      #'   conditions("class", "package") # "package" is ignored with a warning
      #'
      #'   # Do this
      #'   conditions(class = "class", package = "package")
      #'   ```
      #'  }
      #'   \subsection{`cnd:defunct_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:defunct_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:deprecation_warning/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:deprecation_warning`}
      #'     \item{type}{**warning**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:input_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:input_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:input_warning/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:input_warning`}
      #'     \item{type}{**warning**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:invalid_condition/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:invalid_condition`}
      #'     \item{type}{**error**}
      #'   }
      #'   The `class`, `exports`, and `help` parameters must be a single character string.  If you are passing a function, it must be a valid function.
      #'  }
      #'   \subsection{`cnd:invalid_condition_message/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:invalid_condition_message`}
      #'     \item{type}{**error**}
      #'   }
      #'   Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
      #'
      #'   When `message` is not set, a default "there was an error" message is used.
      #'  }
      #'   \subsection{`cnd:match_arg/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:match_arg`}
      #'     \item{type}{**error**}
      #'   }
      #'   Mostly [match.arg()] but with a custom condition
      #'  }
      #'   \subsection{`cnd:match_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:match_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:namespace_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:namespace_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:no_package_exports/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:no_package_exports`}
      #'     \item{type}{**warning**}
      #'   }
      #'   The `exports` parameter requires a `package`
      #'  }
      #'   \subsection{`cnd:path_deletion_message/message`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:path_deletion_message`}
      #'     \item{type}{**message**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:path_overwrite_message/message`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:path_overwrite_message`}
      #'     \item{type}{**message**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:type_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:type_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:value_error/error`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:value_error`}
      #'     \item{type}{**error**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'   \subsection{`cnd:value_warning/warning`}{
      #'   \describe{
      #'     \item{package}{`{cnd}`}
      #'     \item{class}{`cnd:value_warning`}
      #'     \item{type}{**warning**}
      #'   }
      #'   _no help documentation provided_
      #'  }
      #'
      #' @seealso [cnd::cnd-package] [cnd::condition]
      #' @keywords internal
      #'
      NULL

