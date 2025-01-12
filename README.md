
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cnd

<!-- badges: start -->
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

`condition()` creates a `cnd::condition_function` that then generates
the condition when called.

``` r
library(cnd)
bad_value <- condition("bad_value", message = "Value has to be better")

bad_value
#> <<bad_value>>
#>   <type: error>
bad_value()
#> <error/bad_value//Value has to be better: <bad_value>Value has to be better>

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
#> <<bad_value2>>
#>   <type: error>
#> 
#> Generator:
#>   |  x :
bad_value2(0)
#> <error/bad_value2//Value '0' is no good: <bad_value2>Value '0' is no good>

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

Conditions can be assigned into objects within your package by adding
one line to your `.onLoad()`

``` r
.onLoad <- function(libname, pkgname) {
  cnd::register_conditions(pkgname)
}
```

Once that is done, your functions will have a
`cnd::conditioned_function` class added to them and print the conditions
you have linked.

``` r
condition("an_example", pakage = "pkg", exports = "fun")
```

``` r
pkg::fun
function () 
{
    # nonsense example
}
<environment: 0x000000000000>
<condition(s): error/pkg:::an_example>
```
