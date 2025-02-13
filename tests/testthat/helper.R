scrub_environment_code <- function(x) {
  m <- regexpr("(?<=<environment: 0x)[0-9a-f]{12}(?=>)", x, perl = TRUE)
  regmatches(x, m) <- strrep("0", 12L)
  x
}
