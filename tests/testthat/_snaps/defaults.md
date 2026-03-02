# duplicate_error()

    Code
      duplicate_error(x = x)
    Output
      duplicate_error/error
      (duplicate_error/cnd::condition/error/condition)
      Duplicate values found in '`x`': [3] '2'

---

    Code
      duplicate_error(x = y)
    Output
      duplicate_error/error
      (duplicate_error/cnd::condition/error/condition)
      Duplicate values found in '`y`': [2] 'a', [3] 'a', [4] 'a', [5] 'a', [6] 'a', [7] 'a', [8] 'a', [9] 'a', [10] 'a', [11] 'a' (and 485 more)

