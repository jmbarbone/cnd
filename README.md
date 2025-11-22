
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cnd

<!-- badges: start -->

[![R-CMD-check](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/cnd/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/cnd)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `{cnd}` is to provide easy, customized classes for your
`conditions`.

This makes setting up custom conditions quick and more useful.

## Installation

You can install the current CRAN version of `{cnd}` with one of:

``` r
install.packages("cnd")
pak::pak("cran/cnd@*release")
pak::pak("jmbarbone/cnd@*release")
```

You can install the development version of `{cnd}` from
[GitHub](https://github.com/) with:

``` r
pak::pak("jmbarbone/cnd")
```

But is recommended to specify a pre-release tag, if available.

## Conditions, in general

`conditions` are special objects that **R** will use for both signaling
and messaging, primarily within the context of `stop()`, `warning()`,
and `message()`.

``` r
format(stop)[7:10]
#> [1] "        message <- conditionMessage(cond)"               
#> [2] "        call <- conditionCall(cond)"                     
#> [3] "        .Internal(.signalCondition(cond, message, call))"
#> [4] "        .Internal(.dfltStop(message, call))"
format(warning)[9:14]
#> [1] "        message <- conditionMessage(cond)"                   
#> [2] "        call <- conditionCall(cond)"                         
#> [3] "        withRestarts({"                                      
#> [4] "            .Internal(.signalCondition(cond, message, call))"
#> [5] "            .Internal(.dfltWarn(message, call))"             
#> [6] "        }, muffleWarning = function() NULL)"
format(message)[13:19]
#> [1] "    defaultHandler <- function(c) {"                          
#> [2] "        cat(conditionMessage(c), file = stderr(), sep = \"\")"
#> [3] "    }"                                                        
#> [4] "    withRestarts({"                                           
#> [5] "        signalCondition(cond)"                                
#> [6] "        defaultHandler(cond)"                                 
#> [7] "    }, muffleMessage = function() NULL)"
```

All functions accept a `condition` object as their first argument, which
contains both the classes that will be signaled and a message that will
be sent to the `stderr()`. You’ll notice, too, that `warning()` and
`message()` call `withRestarts()`, which contain `signalCondition()`
call. By default, these three functions create `condition` objects with
an extra class of `"error"`, `"warning"`, or `"message"`, respectively.
`{cnd}` inserts itself into these processes by allowing users to define
new condition objects to be signaled and with greater control over
messaging.

## Example

The workhorse of `{cnd}` is `condition()`, which is a special function
of class `cnd::condition_progenitor`, which returns other special
functions of class `cnd::condition_generator`. The
`cnd::condition_generator` objects return `condition`s.

``` r
library(cnd)
condition
#> cnd::condition_progenitor
#> 
#> generator
#>   $ class    : <symbol> 
#>   $ message  : NULL
#>   $ type     : <language> c("condition", "message", "warning", "error")
#>   $ package  : <language> get_package()
#>   $ exports  : NULL
#>   $ help     : NULL
#>   $ registry : <symbol> package
#>   $ register : <language> !is.null(registry)
#> 
#> condition(s)
#> cnd:as_character_cnd_error/error
#> cnd:condition_message_generator/error
#> cnd:condition_overwrite/warning
#> cnd:invalid_condition/error
#> cnd:invalid_condition_message/error
#> cnd:match_arg/error
#> cnd:no_package_exports/warning
#> 
#> For a list of conditions: `cnd::conditions()`
```

> Note: `condition` is of mode “function” but does not retain “function”
> as a class. `condition` also has several conditions which can be
> signaled directly or indirectly.

Use `condition()` to create a *generator*, then use that *generator*
within your functions:

``` r
# cnd::condition_generator
bad_value <- condition(
  "bad_value",
  message = "Value has to be better",
  type = "error"
)

bad_value
#> cnd::condition_generator
#> bad_value/error

# condition
bad_value()
#> bad_value/error
#> (bad_value/cnd::condition/error/condition)
#> Value has to be better

foo <- function(x) {
  if (x < 0) {
    stop(bad_value())
  }
  x
}


foo(-1)
#> Error in foo(): <bad_value>
#> Value has to be better
```

The resulting `cnd::condition_generator` object can also take parameters
that are used in creating a custom message.

``` r
bad_value2 <- condition(
  "bad_value2",
  message = function(x) {
    sprintf("`x` must be `>=0`. A value of `%s` is no good", format(x))
  },
  type = "error"
)

# a 'generator' is also printed, with formals
bad_value2
#> cnd::condition_generator
#> bad_value2/error 
#> 
#> generator
#>   $ x : <symbol>

# pass a value to the args to generate the condition message
bad_value2(0)
#> bad_value2/error
#> (bad_value2/cnd::condition/error/condition)
#> `x` must be `>=0`. A value of `0` is no good
bad_value2(-1)
#> bad_value2/error
#> (bad_value2/cnd::condition/error/condition)
#> `x` must be `>=0`. A value of `-1` is no good

# note: this does not provide any tests, so you may produce non-nonsensical messages
bad_value2(10)
#> bad_value2/error
#> (bad_value2/cnd::condition/error/condition)
#> `x` must be `>=0`. A value of `10` is no good


# now when used in your function:
foo <- function(x) {
  if (x < 0) {
    stop(bad_value2(x))
  }
  x
}

foo(-1.2)
#> Error in foo(): <bad_value2>
#> `x` must be `>=0`. A value of `-1.2` is no good
```

## Your package

There are three things you can do to get the most out of `{cnd}` within
your package.

- Creating a `registry` within your package  
- Assigning a `"condition"` attribute to your functions  
- Documenting your conditions

### Registry

A `registry` is a new environment that will store all of your
conditions. This environment must exist within your package, an `{cnd}`
will be able to find this and use it to connect your conditions to your
functions and to other outputs.

Simple add `cnd_registry()` to an `R/` script in your package. If you
are going to save an store conditions as objects (recommended) then you
should ensure that the `cnd_registry()` call is made before any
conditions are created.

> **NOTE** `cnd_registry()` is designed to use `assign()` within your
> package environment. Please read the documentation to ensure the
> environment is not masked by other objects.

> **NOTE** By default, `condition(registry = )` will pick up on the
> `registry` object within your package when you create your conditions
> and functions are loaded. However, interactive use may not provide the
> same results. See the examples in `cnd_create_registry()` for an
> example of how to create a new registry and assign conditions to the
> registry.

### Assigning conditions

`condition()` has an argument for `exports`, which you can set to any
function which you want to relate with any specific conditions.

If you add any functions to the `exports` option, `package` must be set.
By default, `package` should be set to your development package, so you
don’t need to explicitly assign it every time.

In your package, add `cnd_exports()` to an `R/` script. This should be
executed after all your conditions and their functions are created. This
will add a new `"conditions"` attribute to your functions as well as a
new `"cnd::conditioned_function"` class. The new class specifically
updates the `print()` method to show the conditions assigned to the
function.

### Documentation

When you’ve created your conditions and assigned them to your functions,
you may also want to provide documentation. Or, rather, you should
always provide documentation.

`cnd_document()` will create a new `{package}-cnd-conditions.R` file for
all conditions you have assigned to your package. Simply run the command
when developing to generate a file listing all conditions. You can also
include this after your call to `cnd_exports()` to ensure that all
conditions are documented. The file is written for `{roxygen2}` to
generate the `Rd` files for your package.

``` r
cnd_document()
```

If you want to include other information directly within your roxygen
comments, you can use the `cnd_section()` function to grab all the
conditions from a single functions and print out roxygen-friendly
*section* information:

``` r
cat(cnd_section(cnd))
#> 
#> Conditions are generated through the [`{cnd}`][cnd::cnd-package] package.
#> The following conditions are associated with this function:
#> 
#> \describe{
#>   
#>   \item{[`cnd:cond_cnd_class/error`][cnd-cnd-conditions]}{
#>     [cnd::cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
#>   }
#> 
#> }
#> 
#> For more conditions, see: [cnd-cnd-conditions]
```

Typically, you may want to use this as such:

``` r
#' @section Conditions:
#' `r cnd_section(my_function)`
```

## Retrieval

You can retrieve any `conditions` that are created with `conditions()`.
By default this will list all `conditions` loaded, but can be filtered
by specific packages.

``` r
conditions("cnd", type = "warning")
```

    #> [[1]]
    #> cnd::condition_generator
    #> cnd:cnd_document_conditions/warning 
    #> 
    #> help
    #> Documentation will fail when no conditions are found.  You may be executing [cnd::cnd_document()] too early, before conditions have been registered.  You can try to find your conditions with [cnd::conditions()]. 
    #> 
    #> exports
    #>   cnd::cnd_document()
    #> 
    #> [[2]]
    #> cnd::condition_generator
    #> cnd:condition_overwrite/warning 
    #> 
    #> generator
    #>   $ old : <symbol> 
    #>   $ new : <symbol> 
    #> 
    #> help
    #> Defining a new condition with the same class and package as an existing condition will overwrite the previous definition.  It is recommended to either avoid this by fully defining your condition, or creating a new condition instead. 
    #> 
    #> exports
    #>   cnd::condition()
    #> 
    #> [[3]]
    #> cnd::condition_generator
    #> cnd:conditions_dots/warning 
    #> 
    #> help
    #> The `...` parameter in [cnd::conditions()] is meant for convenience.  Only a single argument is allowed.  Other parameters must be named  explicitly.  For example:  ```r # Instead of this conditions("class", "package") # "package" is ignored with a warning  # Do this conditions(class = "class", package = "package") ``` 
    #> 
    #> exports
    #>   cnd::conditions()
    #> 
    #> [[4]]
    #> cnd::condition_generator
    #> cnd:no_package_exports/warning 
    #> 
    #> help
    #> The `exports` parameter requires a `package` 
    #> 
    #> exports
    #>   cnd::condition()

## `cnd()`

`cnd()` is a special function which will use the appropriate signaling
and handling function based on type of `condition` provided. When the
condition’s type is `"error"` or `"warning"`, `cnd()` passes these
directly through `stop()` and `warning()`, respectively. Both of these
functions have `.Internal()` calls (i.e., `.dfltStop()` and
`.dfltWarn()`), which makes would make them difficult to replicate.
However, `message()` does not, and thus an equivalent wrapper is
internally used which also controls for formatting:

``` r
foo_call <- function() {
  condition("foo_condition", "two\nlines", type = "message")()
}

# provides a character(2) vector output:
conditionMessage(foo_call())
#> [1] "<foo_condition>\ntwo\nlines"
```

`message()` uses a handler which simply collapses the message vector
into a single string. Because of this, the lines are not always neatly
separated:

``` r
message(foo_call())
#> <foo_condition>
#> two
#> lines
```

By contrast, the handlers invoked in `cnd()` will recognize each element
as a separate line for the output. Also, the default is to provide more
information about the call, in a different format:

``` r
cnd(foo_call())
#> <foo_condition>
#> two
#> lines
```

To get the a simpler message, you can use the `options()` function to
change the `cnd.message.format` option to `"simple"`.

``` r
local({
  op <- options(cnd.message.format = "simple", cnd.call = FALSE)
  on.exit(options(op))
  cnd(foo_call())
})
#> <foo_condition>
#> two
#> lines
```

> Currently `message()` and therefore `cnd()` send message conditions to
> the `stderr()`, thus usually giving them an colored text.

Another benefit in using `cnd(condition)` is being able to control for
messages printed to the `stdout()`. Using `cat()` can sometimes create
noise that you’d rather suppress. Because `cnd()` uses an internal
handler for `message` and `condition` types, a condition is signaled
with `singalCondition()`, which can then be caught with calling
handlers, using a provided `"muffleCondition"` restart:

``` r
con <- condition("foo_condition", "Hello\nthere", type = "condition")
my_fun <- function() cnd(con())
my_fun() # note the classes inside (...)
#> foo_condition/condition
#> (foo_condition/cnd::condition/condition)
#> Hello
#> there

withCallingHandlers(
  my_fun(), 
  foo_condition = function(c) {
    tryInvokeRestart("muffleCondition")
  }
)
```
