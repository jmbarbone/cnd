# printing snapshots

    Code
      condition
    Output
      Condition generator
        class    : 
        message  : NULL
        type     : c("error", "warning", "message")
        package  : get_package()
        exports  : NULL
        help     : NULL
        register : !is.null(package)
      <condition(s): cnd:no_package_exports/warning, cnd:invalid_condition_message/error>
      
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
        $ cond: <symbol> 

---

    Code
      cond_condition_overwrite(cond_cnd_class)
    Output
      <cnd:condition_overwrite/warning>
      c("A condition with the class name cnd:cond_cnd_class already exists in NULL and will", " be overwritten")

---

    Code
      cnd
    Output
      function(condition) {
        # should this be raise()?
        # TODO use cond(conition)
        if (!is_cnd_condition(condition)) {
          cnd(cond_cnd_class())
        }
      
        switch(
          attr(condition, "type"),
          error = stop(condition), # maybe `error()` should be the name
          warning = warning(condition),
          message = message(condition)
        )
      }
      <environment: namespace:cnd>
      <condition(s): cnd:cond_cnd_class/error>

