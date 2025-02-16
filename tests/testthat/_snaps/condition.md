# condition(type = 'condition')

    Code
      foo()
    Output
      help:foo/condition
      (help:foo/cnd::condition/condition)
      there was a condition

# .call

    Code
      foo()
    Condition
      Error in `foo()`:
      ! there was an error

---

    Code
      bar()
    Condition
      Error in `foo()`:
      ! there was an error

---

    Code
      foo()
    Condition
      Error:
      ! there was an error

---

    Code
      fizz()
    Condition
      Error in `foo()`:
      ! there was an error

---

    Code
      fizz()
    Condition
      Error in `bar()`:
      ! there was an error

---

    Code
      fizz()
    Condition
      Error in `fizz()`:
      ! there was an error

---

    Code
      fizz()
    Condition
      Error:
      ! there was an error

