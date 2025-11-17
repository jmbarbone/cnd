# printing snapshots

    Code
      condition
    Output
      cnd::condition_progenitor
      
      generator
        $ class    : <symbol> 
        $ message  : NULL
        $ type     : <language> c("condition", "message", "warning", "error")
        $ package  : <language> get_package()
        $ exports  : NULL
        $ help     : NULL
        $ registry : <symbol> package
        $ register : <language> !is.null(registry)
      
      condition(s)
      cnd:as_character_cnd_error/error
      cnd:condition_message_generator/error
      cnd:condition_overwrite/warning
      cnd:invalid_condition/error
      cnd:invalid_condition_message/error
      cnd:match_arg/error
      cnd:no_package_exports/warning
      
      For a list of conditions: `cnd::conditions()`

---

    Code
      cond_cnd_class
    Output
      cnd::condition_generator
      cnd:cond_cnd_class/error 
      
      help
      [cnd::cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()]. 
      
      exports
        cnd::cnd()

---

    Code
      cond_cnd_class()
    Output
      cnd:cond_cnd_class/error
      (cnd:cond_cnd_class/cnd::condition/error/condition)
      'condition' must be a `cnd::condition` object

---

    Code
      cond_condition_overwrite
    Output
      cnd::condition_generator
      cnd:condition_overwrite/warning 
      
      generator
        $ old : <symbol> 
        $ new : <symbol> 
      
      help
      Defining a new condition with the same class and package as an existing condition will overwrite the previous definition.  It is recommended to either avoid this by fully defining your condition, or creating a new condition instead. 
      
      exports
        cnd::condition()

---

    Code
      cond_condition_overwrite(old, new)
    Output
      cnd:condition_overwrite/warning
      (cnd:condition_overwrite/cnd::condition/warning/condition)
      A condition with the class name 'cnd:testing:snapshot_test_old' already exists in 'cnd:testing' and will be overwritten
         1 string mismatch
         target, current do not match when deparsed
         Component ".class": 1 string mismatch
         Component "class": 1 string mismatch
         Component "condition_function": target, current do not match when deparsed
         Component "message": Component "class": 1 string mismatch
         Component "message": Component "original_class": 1 string mismatch
         Component "message": Component "res": 1 string mismatch
         Component "message": Component "res": target, current do not match when deparsed
         Component "class": 1 string mismatch
         Component "condition_env": Component ".class": 1 string mismatch
         Component "condition_env": Component "class": 1 string mismatch
         Component "condition_env": Component "condition_function": target, current do not match when deparsed
         Component "original_class": 1 string mismatch
         Component "res": 1 string mismatch
         Component "res": target, current do not match when deparsed

---

    Code
      fun
    Output
      function() NULL
      <environment: 0x000000000000>
      
      condition(s)
      test-snapshots:snapshot_test_fun/condition

---

    Code
      cond_condition_bad_message
    Output
      cnd::condition_generator
      cnd:invalid_condition_message/error 
      
      help
      Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.  When `message` is not set, a default "there was an error" message is used. 
      
      exports
        cnd::condition()

# printing with cli

    Code
      condition
    Output
      cnd::condition_progenitor
      
      generator
        $ class    : <symbol> 
        $ message  : NULL
        $ type     : <language> c("condition", "message", "warning", "error")
        $ package  : <language> get_package()
        $ exports  : NULL
        $ help     : NULL
        $ registry : <symbol> package
        $ register : <language> !is.null(registry)
      
      condition(s)
    Message
      `cnd:as_character_cnd_error/error`
      `cnd:condition_message_generator/error`
      `cnd:condition_overwrite/warning`
      `cnd:invalid_condition/error`
      `cnd:invalid_condition_message/error`
      `cnd:match_arg/error`
      `cnd:no_package_exports/warning`
    Output
      
    Message
      For a list of conditions: `cnd::conditions()`

