# Changelog

## cnd (development version)

- `condition(class = )` is now deprecated; instead `condition(name = )`
  should be use; `name` is now the first argument
- `condition(classes = )` is added to include additional condition
  classes
- adds default conditions with optional dynamic condition messages;
  these are defaults without `package = "cnd"`
  [\#4](https://github.com/jmbarbone/cnd/issues/4)
  - [`value_error()`](https://jmbarbone.github.io/cnd/reference/defaults.md),
    [`value_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
  - [`class_error()`](https://jmbarbone.github.io/cnd/reference/defaults.md),
    [`class_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
  - [`type_error()`](https://jmbarbone.github.io/cnd/reference/defaults.md),
    [`type_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
  - [`input_error()`](https://jmbarbone.github.io/cnd/reference/defaults.md),
    [`input_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
  - [`use_error()`](https://jmbarbone.github.io/cnd/reference/defaults.md),
    [`use_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
    (i.e., /juːs/, not /juːz/)
  - `default_error()`
  - [`deprecated_warning()`](https://jmbarbone.github.io/cnd/reference/defaults.md)
- [`cnd_document()`](https://jmbarbone.github.io/cnd/reference/cnd_document.md)’s
  links to [cnd](https://jmbarbone.github.io/cnd/) are corrected
  [\#25](https://github.com/jmbarbone/cnd/issues/25)
- `condition(message = )` now handles `...` and other arguments
  correctly [\#23](https://github.com/jmbarbone/cnd/issues/23)

## cnd 0.1.1

- `cnd::condition_progenitor$message` no longer has its environment
  reset [\#14](https://github.com/jmbarbone/cnd/issues/14)
- test fix for [testthat](https://testthat.r-lib.org) 3.3.0
  [\#16](https://github.com/jmbarbone/cnd/issues/16)
  [@hadley](https://github.com/hadley)
- additional *help* information provided for conditions
  [\#5](https://github.com/jmbarbone/cnd/issues/5)

## cnd 0.1.0

- Initial release
