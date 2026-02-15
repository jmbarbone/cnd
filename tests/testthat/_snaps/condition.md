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
      ! <foo>
      there was an error

---

    Code
      bar()
    Condition
      Error in `foo()`:
      ! <foo>
      there was an error

---

    Code
      foo2()
    Condition
      Error in `foo2()`:
      ! could not find function "foo2"

---

    Code
      fizz()
    Condition
      Error in `foo()`:
      ! <foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error in `bar()`:
      ! <foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error in `fizz()`:
      ! <foo>
      there was an error

---

    Code
      fizz()
    Condition
      Error:
      ! <foo>
      there was an error

