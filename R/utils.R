`%or%` <- function(x, y) {
  if (length(x) == 0) y else x
}

str_extract_all <- function(x, pattern, invert = FALSE) {
  regmatches(x, gregexpr(pattern, x, perl = TRUE), invert)[[1]]
}

str_replace_all <- function(x, pattern, replacement) {
  regex <- gregexpr(pattern, x, perl = TRUE)
  regmatches(x, regex)[[1]] <- replacement
  x
}
