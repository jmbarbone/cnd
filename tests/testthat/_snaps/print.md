# printing snapshots

    Code
      condition
    Output
      Condition generator
        class    : 
        message  : NULL
        type     : c("error", "warning", "message", "condition")
        package  : get_package()
        exports  : NULL
        help     : NULL
        register : !is.null(package)
      <condition(s): cnd:no_package_exports/warning, cnd:invalid_condition_message/error, cnd:invalid_condition/error>
      
      For list of conditions use cnd::conditions()

---

    Code
      cond_cnd_class
    Output
      <cnd:cond_cnd_class/error>
      
      `cnd()` simple calls the appropriate function: `stop()`, `warning()`, or
      `message()` based on the `type` parameter from `cnd::condition()`
      
      exports:
        cnd::cnd

---

    Code
      cond_cnd_class()
    Output
      <cnd:cond_cnd_class/error>
      'condition' must be a `cnd::condition` object

---

    Code
      cond_condition_overwrite
    Output
      <cnd:condition_overwrite/warning>
      
      generator:
        $ old: <symbol> 
        $ new: <symbol> 

---

    Code
      cond_condition_overwrite(old, new)
    Output
      <cnd:condition_overwrite/warning>
      A condition with the class name 'cnd:testing:snapshot_test_old' already exists in 'cnd:testing' and will be overwritten   1 string mismatch   target, current do not match when deparsed   Component ".class": 1 string mismatch   Component "class": 1 string mismatch   Component "condition_function": target, current do not match when deparsed   Component ".class": 1 string mismatch   Component "class": 1 string mismatch   Component "condition_function": target, current do not match when deparsed

