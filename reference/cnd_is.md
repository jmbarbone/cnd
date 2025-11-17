# `is` functions for [cnd](https://jmbarbone.github.io/cnd/reference/condition.md)

`is` functions for
[cnd](https://jmbarbone.github.io/cnd/reference/condition.md)

## Usage

``` r
is_condition(x)

is_cnd_condition(x)

is_cnd_generator(x, type = c("error", "warning", "message", "condition"))

is_conditioned_function(x)
```

## Arguments

- x:

  An object

- type:

  A specific type to check

## Value

`TRUE` or `FALSE` for the test

## Examples

``` r
is_condition(simpleCondition(""))
#> [1] TRUE
is_cnd_condition(simpleCondition(""))
#> [1] FALSE

con <- condition("is")
is_condition(con)
#> [1] FALSE
is_cnd_condition(con)
#> [1] FALSE

is_condition(con())
#> [1] TRUE
is_cnd_condition(con())
#> [1] TRUE

is_cnd_generator(con)
#> [1] TRUE

is_conditioned_function(cnd)
#> [1] TRUE
```
