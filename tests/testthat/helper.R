scrub_environment_code <- function(x) {
  # macos: 9
  # linux: 12
  # windows: 16
  m <- regexpr("(?<=<environment: 0x)[0-9a-f]+(?=>)", x, perl = TRUE)
  regmatches(x, m) <- strrep("0", max(0, attr(m, "match.length")))
  x
}
