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

replace_substr <- function(x, start, stop, replacement) {
  
  paste0(
    substr(x, 0, start - 1),
    replacement,
    substr(x, stop + 1, nchar(x))
  )
  
}

parse_code <- function(code) {
  
  force(code)
  
  env <- caller_env()
  
  tryCatch(
    utils::getParseData(
      parse(text = code, keep.source = TRUE), 
      includeText = TRUE
    ),
    error = function(e) {
      msg <- paste(e$message, collapse = "")
      abort(
        c("Failed to parse code",
          i = paste("Check", msg)),
        call = env
      )
    }
  )

  
}