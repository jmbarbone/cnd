
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cnd

<!-- badges: start -->

[![R-CMD-check](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/cnd/actions/workflows/R-CMD-check.yaml)
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

`condition()` creates a `cnd::condition_function` that can generate the
condition when called.

``` r
library(cnd)
bad_value <- condition("bad_value", message = "Value has to be better")

bad_value
#> <error/bad_value>
bad_value()
#> <error/bad_value> Value has to be better

foo <- function(x) {
  if (x < 0) {
    stop(bad_value())
  }
  x
}

try(foo(-1))
#> Error : <bad_value>
#>  Error : Value has to be better
```

`cnd::condition_function`s can also take parameters that are used in
creating a custom message.

``` r
bad_value2 <- condition(
  "bad_value2",
  message = function(x) sprintf("Value '%s' is no good", format(x))
)

bad_value2
#> <error/bad_value2>
#> 
#> generator:
#>   $ x: <symbol>
bad_value2(0)
#> <error/bad_value2> Value '0' is no good

foo <- function(x) {
  if (x < 0) {
    stop(bad_value2(x))
  }
  x
}

try(foo(-1L))
#> Error : <bad_value2>
#>  Error : Value '-1' is no good
```

## Registration

Conditions can be assigned into objects within your package by adding
one line to your package. This is probably best in a file like `R/zzz.R`
that can be run at the end of your package load, before the namespace is
locked.

``` r
cnd::register_conditions(pkgname)
```

Once that is done, your functions will have a
`cnd::conditioned_function` class added to them and print the conditions
you have linked.

``` r
condition(
  "an_example",
  package = "pkg",
  help = "more info",
  exports = "fun"
)
#> <pkg:error/an_example>
#> 
#> more info
#> 
#> exports:
#>   pkg::fun
```

``` r
pkg::fun
#> function () 
#> {
#>     # nonsense example
#> }
#> <environment: 0x000000000000>
#> <condition(s): pkg:error/an_example>
```

## Documentation

`cnd_document()` will create a `.Rd` file for all conditions you have
assigned to your package. Simply run the command when developing to
generate a file listing all conditions.

``` r
cnd::cnd_document()
```

## Retrieval

You can retrieve any `conditions` that are created with `conditions()`.
By default this will list all `conditions` loaded, but can be filtered
by specific packages.

``` r
conditions("cnd")
```

    #> [[1]]
    #> <cnd:error/cond_bad_message>
    #> 
    #> Conditions messages are displayed when invoked through conditionMessage().
    #> You can set a static message by passing through a `character` vector, or a
    #> dynamic message by passing through a `function`.  The function should return
    #> a `character` vector.
    #> 
    #> When `message` is not set, a default "there was an error" message is used.
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[2]]
    #> <cnd:error/as_character_cnd_error>
    #> 
    #> You cannot coerce a `cnd::condition_function` object to a character.  This
    #> may have occured when trying to put a condition function through `stop()` or
    #> `warning`.  Instead, call the function first, then pass the result to
    #> `stop()` or `warning()`.
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
    #> [[3]]
    #> <cnd:warning/cond_no_package_exports>
    #> 
    #> The `exports` parameter requires a `package`
    #> 
    #> exports:
    #>   cnd::condition
    #> 
    #> [[4]]
    #> <cnd:error/cond_cnd_class>
    #> 
    #> `cnd()` simple calls the appropriate function: `stop()`, `warning()`, or
    #> `message()` based on the `type` parameter from `cnd::condition()`
    #> 
    #> exports:
    #>   cnd::cnd
    #> 
    #> [[5]]
    #> <cnd:warning/condition_overwrite>
    #> 
    #> generator:
    #>   $ cond: <symbol>
