# cnd (development version)

- `condition(class = )` is now deprecated; instead `condition(name = )` should be use; `name` is now the first argument
- `condition(classes = )` is added to include additional condition classes
- adds default conditions with optional dynamic condition messages; these are defaults without `package = "cnd"` [#4](https://github.com/jmbarbone/cnd/issues/4)
  - `value_error()`, `value_warning()`
  - `class_error()`, `class_warning()`
  - `type_error()`, `type_warning()`
  - `input_error()`, `input_warning()`
  - `use_error()`, `use_warning()` (i.e., /juːs/, not /juːz/)
  - `default_error()`
  - `deprecated_warning()`
- `cnd_document()`'s links to `{cnd}` are corrected [#25](https://github.com/jmbarbone/cnd/issues/25)
- `condition(message = )` now handles `...` and other arguments correctly [#23](https://github.com/jmbarbone/cnd/issues/23) 

# cnd 0.1.1

- `cnd::condition_progenitor$message` no longer has its environment reset [#14](https://github.com/jmbarbone/cnd/issues/14)
- test fix for `{testthat}` 3.3.0 [#16](https://github.com/jmbarbone/cnd/issues/16) [@hadley](https://github.com/hadley)
- additional _help_ information provided for conditions [#5](https://github.com/jmbarbone/cnd/issues/5)

# cnd 0.1.0

- Initial release
