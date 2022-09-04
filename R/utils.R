`%or%` <- function(x, y) {
  if (length(x) == 0) y else x
}

# strip_quotes(c("'`backticks and single`'", '"double"', "`one backtick"))
strip_quotes <- function(x, quotes = c('"', "'", "`"), passes = 1) {
  
  if (length(x) == 0) {
    return(x)
  }
  
  passes <- min(passes, length(quotes))
  
  is_quoted <- function(x) {
    tests <- lapply(quotes, function(q) startsWith(x, q) & endsWith(x, q))
    Reduce(`|`, tests)
  }
  
  unquote <- function(x) substr(x, 2, nchar(x) - 1)
  
  pass_n <- rep(1, length(x))
  
  for (i in seq_len(passes)) {
    run <- is_quoted(x) & i <= passes
    x <- ifelse(run, unquote(x), x)
    pass_n <- pass_n + as.numeric(run)
  }
  
  x
  
}

