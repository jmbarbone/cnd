# cnd_message() works

    Code
      cnd_message(foo(), "simple")
    Message
      there was a message
    Output
      NULL

---

    Code
      cnd_message(foo(), "verbose")
    Message
      there was a message
    Output
      NULL

# cnd_condition() works

    Code
      cnd_condition(foo(), "simple")
    Output
      <cnd:foo>
      there was a condition

---

    Code
      cnd_condition(foo(), "verbose")
    Output
      cnd:foo/condition
      (cnd:foo/cnd::condition/condition)
      there was a condition

