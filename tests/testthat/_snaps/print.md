# printing snapshots

    Code
      condition
    Output
      cnd::condition_progenitor
      
      generator
        $ name     : <symbol> 
        $ message  : NULL
        $ type     : <language> c("condition", "message", "warning", "error")
        $ package  : <language> get_package()
        $ exports  : NULL
        $ help     : NULL
        $ registry : <symbol> package
        $ register : <language> !is.null(registry)
        $ classes  : NULL
        $ class    : <symbol> 
      
      condition(s)
      cnd:condition_as_character_error/error
      cnd:condition_message_error/error
      cnd:condition_message_generator_error/error
      cnd:condition_overwrite_warning/warning
      cnd:invalid_condition_error/error
      cnd:match_arg/error
      cnd:no_package_exports_warning/warning
      
      For a list of conditions: `cnd::conditions()`

---

    Code
      cnd_class_error
    Output
      cnd::condition_generator
      cnd:cnd_class_error/error 
      
      help
      [cnd::cnd()] simple calls the appropriate function: [base::stop()], [base::warning()], or [base::message()] based on the `type` parameter from [cnd::condition()]. 
      
      exports
        cnd::cnd()

---

    Code
      cnd_class_error()
    Output
      cnd:cnd_class_error/error
      (cnd:cnd_class_error/cnd::condition/input_error/error/condition)
      'condition' must be a `cnd::condition` object

---

    Code
      condition_overwrite_warning
    Output
      cnd::condition_generator
      cnd:condition_overwrite_warning/warning 
      
      generator
        $ old : <symbol> 
        $ new : <symbol> 
      
      exports
        cnd::condition()

---

    Code
      condition_overwrite_warning(old, new)
    Output
      cnd:condition_overwrite_warning/warning
      (cnd:condition_overwrite_warning/cnd::condition/warning/condition)
      A condition with the class name 'cnd:testing:snapshot_test_old' already exists in 'cnd:testing' and will be overwritten
         1 string mismatch
         target, current do not match when deparsed
         Component "class": 1 string mismatch
         Component "condition_function": target, current do not match when deparsed
         Component "message": Component "name": 1 string mismatch
         Component "message": Component "original_class": 1 string mismatch
         Component "message": Component "res": 1 string mismatch
         Component "message": Component "res": target, current do not match when deparsed
         Component "message": Component "res": 1 string mismatch
         Component "original_class": 1 string mismatch
         Component "condition_env": Component "class": 1 string mismatch
         Component "condition_env": Component "condition_function": target, current do not match when deparsed
         Component "condition_env": Component "original_class": 1 string mismatch
         Component "name": 1 string mismatch
         Component "original_class": 1 string mismatch
         Component "res": 1 string mismatch
         Component "res": target, current do not match when deparsed
         Component "res": 1 string mismatch
         1 string mismatch

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
      condition_message_error
    Output
      cnd::condition_generator
      cnd:condition_message_error/error 
      
      help
      Conditions messages are displayed when invoked through [base::conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.  When `message` is not set, a default "there was an error" message is used. 
      
      exports
        cnd::condition()

# printing with cli

    Code
      condition
    Output
      cnd::condition_progenitor
      
      generator
        $ name     : <symbol> 
        $ message  : NULL
        $ type     : <language> c("condition", "message", "warning", "error")
        $ package  : <language> get_package()
        $ exports  : NULL
        $ help     : NULL
        $ registry : <symbol> package
        $ register : <language> !is.null(registry)
        $ classes  : NULL
        $ class    : <symbol> 
      
      condition(s)
    Message
      `cnd:condition_as_character_error/error`
      `cnd:condition_message_error/error`
      `cnd:condition_message_generator_error/error`
      `cnd:condition_overwrite_warning/warning`
      `cnd:invalid_condition_error/error`
      `cnd:match_arg/error`
      `cnd:no_package_exports_warning/warning`
    Output
      
    Message
      For a list of conditions: `cnd::conditions()`

