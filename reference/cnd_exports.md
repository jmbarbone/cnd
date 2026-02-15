# Add conditions to functions

`[cnd::cnd_exports()]` should be used within a package's building
environment.

## Usage

``` r
cnd_exports(env = parent.frame())
```

## Arguments

- env:

  The package environment

## Value

Nothing, called for its side-effects

## Examples

``` r
e <- new.env()
registry <- cnd_create_registry("EXAMPLE", env = e)
local(envir = e, {
  my_fun <- function() NULL
  condition(
    "my_condition",
    package = "example_package",
    exports = "my_fun",
    registry = registry
  )
  cnd_exports()
})

# conditions are now added to my_fun():
e$my_fun
#> function () 
#> NULL
#> <environment: 0x55605b5b7388>
#> 
#> condition(s)
#> `example_package:my_condition/condition`
conditions(e$my_fun)
#> [[1]]
#> cnd::condition_generator
#> example_package:my_condition/condition 
#> 
#> exports
#>   example_package::my_fun()
#> 
```
