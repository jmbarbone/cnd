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
      ! <cnd:foo>
      there was an error

---

    Code
      bar()
    Condition
      Error in `foo()`:
      ! <cnd:foo>
      there was an error

---

    Code
      foo()
    Condition
      Error:
      ! <cnd:foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error in `foo()`:
      ! <cnd:foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error in `bar()`:
      ! <cnd:foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error in `fizz()`:
      ! <cnd:foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error:
      ! <cnd:foo>
      there was an error

