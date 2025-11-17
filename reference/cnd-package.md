# cnd: Create and Register Conditions

An interface for creating new condition generators objects. Generators
are special functions that can be saved in registries and linked to
other functions. Utilities for documenting your generators, and new
conditions is provided for package development.

## Options

- `cnd.cli.override`  
  `["on"|"off"|"none"]`:

  Controls override for printing messages from `{cli}`. When `"on"` or
  `"off"`, messages will be overridden to that state. When `"none"`,
  `{cli}` will be used to determine appropriate state.

- `cnd.condition.message`  
  `["verbose"|"simple"]`:

  How messages should be printed from conditions. `"verbose"` will
  provide the message, information about the condition's class, and call
  information. `"simple"` will only provide the message.

- `cnd.call`  
  `[TRUE|FALSE]`:

  Whether to print the call that generated the condition. This is
  embedded within the
  [`conditionCall()`](https://rdrr.io/r/base/conditions.html) method.

## See also

[`condition()`](https://jmbarbone.github.io/cnd/reference/condition.md)
[cnd-cnd-conditions](https://jmbarbone.github.io/cnd/reference/cnd-cnd-conditions.md)

## Author

**Maintainer**: Jordan Mark Barbone <jmbarbone@gmail.com>
([ORCID](https://orcid.org/0000-0001-9788-3628)) \[copyright holder\]
