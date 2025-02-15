# printing snapshots

    Code
      condition
    Output
      cnd::condition_progenitor
      generator:
        $ class   : <symbol> 
        $ message : NULL
        $ type    : <language> c("error", "warning", "message", "condition")
        $ package : <language> get_package()
        $ exports : NULL
        $ help    : NULL
        $ registry: <symbol> package
        $ register: <language> !is.null(registry)
      
      <condition(s): cnd:as_character_cnd_error/error, cnd:invalid_condition/error, cnd:invalid_condition_message/error, cnd:match_arg/error, cnd:no_package_exports/warning>
      
      For list of conditions use cnd::conditions()

---

    Code
      cond_cnd_class
    Output
      cnd::condition_generator
      <cnd:cond_cnd_class/error>
      
      [cnd()] simple calls the appropriate function: [stop()], [warning()], or [message()] based on the `type` parameter from [cnd::condition()].
      
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
      cnd::condition_generator
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

---

    Code
      fun
    Output
      function() NULL
      <environment: 0x000000000000>
      <condition(s): test-snapshots:snapshot_test_fun/error>

---

    Code
      cond_condition_bad_message
    Output
      cnd::condition_generator
      <cnd:invalid_condition_message/error>
      
      Conditions messages are displayed when invoked through [conditionMessage()].  You can set a static message by passing through a `character` vector, or a dynamic message by passing through a `function`.  The function should return a `character` vector.
      
      When `message` is not set, a default "there was an error" message is used.
      
      exports:
        cnd::condition

