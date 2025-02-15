
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cnd

<!-- badges: start -->

[![R-CMD-check](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/cnd/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/cnd)
<!-- badges: end -->

The goal of `{cnd}` is to provide easy, customized classes for
`conditions`. This makes setting up custom conditions quick and more
useful.

## Installation

You can install the development version of `{cnd}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jmbarbone/cnd")
```

## Example

The workhorse of `{cnd}` is `condition()`, which is a special function
of class `cnd::condition_progenitor`, which returns other special
functions of class `cnd::condition_generator`. The
`cnd::condition_generator` objects return `condition`s.

``` r
library(cnd)
condition
#> cnd::condition_progenitor
#> generator:
#>   $ class   : <symbol> 
#>   $ message : NULL
#>   $ type    : <language> c("error", "warning", "message", "condition")
#>   $ package : <language> get_package()
#>   $ exports : NULL
#>   $ help    : NULL
#>   $ registry: <symbol> package
#>   $ register: <language> !is.null(registry)
#> 
#> <condition(s): cnd:as_character_cnd_error/error, cnd:invalid_condition/error, cnd:invalid_condition_message/error, cnd:match_arg/error, cnd:no_package_exports/warning>
#> 
#> For list of conditions use cnd::conditions()
```

Use `condition()` to create a generator, which controls messaging for
`conditions`s.

``` r
bad_value <- condition("bad_value", message = "Value has to be better")

bad_value
#> cnd::condition_generator
#> <bad_value/error>
bad_value()
#> <bad_value/error>
#> Value has to be better

foo <- function(x) {
  if (x < 0) {
    cnd(bad_value())
  }
  x
}


foo(-1)
#> Error in foo(-1): <bad_value>
#> Error in foo(-1): Value has to be better
```

By default, the `call` will try to still grab the from the sys.parent of
`bad_value()`. As `bad_value()` is called inside `foo()`, we still see
`foo()` in the error.

``` r
bar <- function() foo(-1L)
bar()
#> Error in foo(-1L): <bad_value>
#> Error in foo(-1L): Value has to be better
```

The resulting `cnd::condition_generator` object can also take parameters
that are used in creating a custom message.

``` r
bad_value2 <- condition(
  "bad_value2",
  message = function(x) {
    sprintf("`x` must be `>=0`. A value of `%s` is no good", format(x))
  }
)

bad_value2
#> cnd::condition_generator
#> <bad_value2/error>
#> generator:
#>   $ x: <symbol>
bad_value2(0)
#> <bad_value2/error>
#> `x` must be `>=0`. A value of `0` is no good

foo <- function(x) {
  if (x < 0) {
    stop(bad_value2(x))
  }
  x
}

foo(-1.2)
#> Error in foo(-1.2): <bad_value2>
#> Error in foo(-1.2): `x` must be `>=0`. A value of `-1.2` is no good
```

## Registration

`condition()` has a few options for *registering* a condition to an
exported `function`. When using `condition()` within your package
development, you should only need to use the `exports` parameter, which
requires a `character` vector of exported function. `register` toggles
whether a `cnd::condition_generator` is assigned to an object.
`registry` is for advanced use cases where you want to save the
`cnd::condition_generator` in a registration environment that is not
associated with your package.

``` r
condition
#> cnd::condition_progenitor
#> generator:
#>   $ class   : <symbol> 
#>   $ message : NULL
#>   $ type    : <language> c("error", "warning", "message", "condition")
#>   $ package : <language> get_package()
#>   $ exports : NULL
#>   $ help    : NULL
#>   $ registry: <symbol> package
#>   $ register: <language> !is.null(registry)
#> 
#> <condition(s): cnd:as_character_cnd_error/error, cnd:invalid_condition/error, cnd:invalid_condition_message/error, cnd:match_arg/error, cnd:no_package_exports/warning>
#> 
#> For list of conditions use cnd::conditions()
```

When conditions are registered to a function, they can be retrieved from
that function. `condition()` itself has several
`cnd::condition_generator` objects.

``` r
conditions(condition)
```

    #> [[1]]
    #> cnd::condition_generator
    #> <cnd:as_character_cnd_error/error>
    #> 
    #> You cannot coerce a [cnd::condition_generator] object to a character. This may have occured when trying to put a condition function through [stop()] or [warning].  Instead, call the function first, then pass the result to [stop()] or [warning()].
    #> 
    #> For example:
    #> 
    #> ```r
    #> # Instead of this
    #> stop(my_condition)
    #> 
    #> # Do this
    #> stop(my_condition())
    #> ```
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[2]]
    #> cnd::condition_generator
    #> <cnd:invalid_condition/error>
    #> generator:
    #>   $ problems: <symbol> 
    #> 
    #> The `class`, `exports`, and `help` parameters must be a single character string.  If you are passing a function, it must be a valid function.
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[3]]
    #> cnd::condition_generator
    #> <cnd:invalid_condition_message/error>
    #> 
    #> Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
    #> 
    #> When `message` is not set, a default "there was an error" message is used.
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[4]]
    #> cnd::condition_generator
    #> <cnd:match_arg/error>
    #> generator:
    #>   $ arg    : <symbol> 
    #>   $ value  : <symbol> 
    #>   $ choices: <symbol> 
    #> 
    #> Mostly [match.arg()] but with a custom condition
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[5]]
    #> cnd::condition_generator
    #> <cnd:no_package_exports/warning>
    #> 
    #> The `exports` parameter requires a `package`
    #> 
    #> exports:
    #>   cnd::condition

<!-- These can be retrieved through their names: -->
<!-- ```{r} -->
<!-- cond("cnd:invalid_condition_message") -->
<!-- ``` -->

To get the most out of your development package, add a call to
`cnd::cnd_exports()` in your last sourced filed. Usually saved in
`R/zzz.R` should be good enough.

## Documentation

`cnd_document()` will create a new `.R` file for all conditions you have
assigned to your package. Simply run the command when developing to
generate a file listing all conditions.

``` r
cnd::cnd_document()
```

This can also be called after your `cnd::cnd_exports()`.

There’s a specialist `cnd_section()` function which can be used within a
`roxygen` block inside your own documentation. This returns `rogxygen2`
friendly text that can also copy information about your conditions to
documentation for your functions.

``` r
cat(cnd_section("cnd"))
```

    #> 
    #> Conditions are generated through the [`{cnd}`][cnd::cnd-package] package.
    #> The following conditions are associated with this function:
    #> 
    #> \describe{
    #>   
    #>   \item{[`cnd:as_character_cnd_error/error`][cnd-cnd-conditions]}{
    #>     You cannot coerce a [cnd::condition_generator] object to a character. This may have occured when trying to put a condition function through [stop()] or [warning].  Instead, call the function first, then pass the result to [stop()] or [warning()].
    #> 
    #> For example:
    #> 
    #> ```r
    #> # Instead of this
    #> stop(my_condition)
    #> 
    #> # Do this
    #> stop(my_condition())
    #> ```
    #>   }
    #> 
    #>   \item{[`cnd:cnd_generated_cleanup/condition`][cnd-cnd-conditions]}{
    #>     
    #>   }
    #> 
    #>   \item{[`cnd:cnd_generated_write/condition`][cnd-cnd-conditions]}{
    #>     
    #>   }
    #> 
    #>   \item{[`cnd:cond_cnd_class/error`][cnd-cnd-conditions]}{
    #>     [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
    #>   }
    #> 
    #>   \item{[`cnd:condition_overwrite/warning`][cnd-cnd-conditions]}{
    #>     
    #>   }
    #> 
    #>   \item{[`cnd:conditions_dots/warning`][cnd-cnd-conditions]}{
    #>     The `...` parameter in [conditions()] is meant for convenience.  Onlya single argument is alowed.  Other parameters must be named  explicitly.For example:```r# Instead of thisconditions('class', 'package') # 'package' is ignored with a warning# Do thisconditions(class = 'class', package = 'package')```
    #>   }
    #> 
    #>   \item{[`cnd:invalid_condition/error`][cnd-cnd-conditions]}{
    #>     The `class`, `exports`, and `help` parameters must be a single character string.  If you are passing a function, it must be a valid function.
    #>   }
    #> 
    #>   \item{[`cnd:invalid_condition_message/error`][cnd-cnd-conditions]}{
    #>     Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
    #> 
    #> When `message` is not set, a default "there was an error" message is used.
    #>   }
    #> 
    #>   \item{[`cnd:match_arg/error`][cnd-cnd-conditions]}{
    #>     Mostly [match.arg()] but with a custom condition
    #>   }
    #> 
    #>   \item{[`cnd:no_package_exports/warning`][cnd-cnd-conditions]}{
    #>     The `exports` parameter requires a `package`
    #>   }
    #> 
    #> }
    #> 
    #> For more conditions, see: [cnd-cnd-conditions]

## Retrieval

You can retrieve any `conditions` that are created with `conditions()`.
By default this will list all `conditions` loaded, but can be filtered
by specific packages.

``` r
conditions("cnd")
```

    #> [[1]]
    #> cnd::condition_generator
    #> <cnd:as_character_cnd_error/error>
    #> 
    #> You cannot coerce a [cnd::condition_generator] object to a character. This may have occured when trying to put a condition function through [stop()] or [warning].  Instead, call the function first, then pass the result to [stop()] or [warning()].
    #> 
    #> For example:
    #> 
    #> ```r
    #> # Instead of this
    #> stop(my_condition)
    #> 
    #> # Do this
    #> stop(my_condition())
    #> ```
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[2]]
    #> cnd::condition_generator
    #> <cnd:cnd_generated_cleanup/condition>
    #> generator:
    #>   $ paths: <symbol> 
    #> 
    #> exports:
    #>   cnd::cnd_document
    #> 
    #> [[3]]
    #> cnd::condition_generator
    #> <cnd:cnd_generated_write/condition>
    #> generator:
    #>   $ path: <symbol> 
    #> 
    #> exports:
    #>   cnd::cnd_document
    #> 
    #> [[4]]
    #> cnd::condition_generator
    #> <cnd:cond_cnd_class/error>
    #> 
    #> [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
    #> 
    #> exports:
    #>   cnd::cnd
    #> 
    #> [[5]]
    #> cnd::condition_generator
    #> <cnd:condition_overwrite/warning>
    #> generator:
    #>   $ old: <symbol> 
    #>   $ new: <symbol> 
    #> 
    #> [[6]]
    #> cnd::condition_generator
    #> <cnd:conditions_dots/warning>
    #> 
    #> The `...` parameter in [conditions()] is meant for convenience.  Onlya single argument is alowed.  Other parameters must be named  explicitly.For example:```r# Instead of thisconditions('class', 'package') # 'package' is ignored with a warning# Do thisconditions(class = 'class', package = 'package')```
    #> 
    #> exports:
    #>   cnd::conditions
    #> 
    #> [[7]]
    #> cnd::condition_generator
    #> <cnd:invalid_condition/error>
    #> generator:
    #>   $ problems: <symbol> 
    #> 
    #> The `class`, `exports`, and `help` parameters must be a single character string.  If you are passing a function, it must be a valid function.
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[8]]
    #> cnd::condition_generator
    #> <cnd:invalid_condition_message/error>
    #> 
    #> Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
    #> 
    #> When `message` is not set, a default "there was an error" message is used.
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[9]]
    #> cnd::condition_generator
    #> <cnd:match_arg/error>
    #> generator:
    #>   $ arg    : <symbol> 
    #>   $ value  : <symbol> 
    #>   $ choices: <symbol> 
    #> 
    #> Mostly [match.arg()] but with a custom condition
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[10]]
    #> cnd::condition_generator
    #> <cnd:no_package_exports/warning>
    #> 
    #> The `exports` parameter requires a `package`
    #> 
    #> exports:
    #>   cnd::condition
