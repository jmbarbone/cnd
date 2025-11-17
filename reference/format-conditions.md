# Format conditions

Formats
[condition](https://jmbarbone.github.io/cnd/reference/condition.md)
objects

## Usage

``` r
# S3 method for class '`cnd::condition`'
format(x, ..., cli = getOption("cnd.cli.override"))

# S3 method for class '`cnd::condition_generator`'
format(x, ..., cli = getOption("cnd.cli.override"))
```

## Arguments

- x:

  A [condition](https://jmbarbone.github.io/cnd/reference/condition.md)
  object

- ...:

  Not used

- cli:

  If `TRUE` will use formatting from
  [cli](https://cli.r-lib.org/reference/cli-package.html). Default uses
  an option, `"cnd.cli.override"`, if available, otherwise checks that
  `cli` is installed and ansi colors are available.

## Value

A `character` vector

## Examples

``` r
format(condition("foo"))
#> [1] "foo/\033[90mcondition\033[39m"
```
