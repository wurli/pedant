`%or%` <- function(x, y) {
  if (length(x) == 0) y else x
}

`%if_na%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}

# strip_quotes(c("'`backticks and single`'", '"double"', "`one backtick"))
strip_quotes <- function(x, quotes = c('"', "'", "`"), passes = 1) {
  
  if (length(x) == 0) {
    return(x)
  }
  
  passes <- min(passes, length(quotes))
  
  is_quoted <- function(x) {
    tests <- map(quotes, function(q) startsWith(x, q) & endsWith(x, q))
    reduce(tests, `|`)
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

count <- function(x, ..., .wt = NULL) {
  
  cols <- c(...)
  
  levels <- do.call(paste, x[cols])
  
  groups <- split(x, levels)
  
  counted_groups <- map(groups, function(df) {
    
    .wt <- if (is.null(.wt)) 1 else sum(df[[.wt]])
    
    cbind(
      df[1, cols], 
      data.frame(n = nrow(df) * .wt)
    )
  })
  
  unordered_result <- do.call(rbind, counted_groups)
  
  unordered_result[do.call(order, unordered_result), , drop = FALSE]
  
}

comma <- function(x, fixed_width = FALSE) {
  format(x, big.mark = ",", scientific = FALSE, trim = !fixed_width)
}

justify <- function(x, just = c("left", "right", "centre")) {
  
  chars  <- nchar(x)
  n      <- max(chars) - chars
  spaces <- function(x) strrep(" ", x) 
  
  switch(
    just[1],
    left   = paste0(x, spaces(n)),
    right  = paste0(spaces(n), x),
    centre = paste0(spaces(floor(n / 2)), x, spaces(ceiling(n / 2)))
  )
  
}

style_pedant_run <- function(code, text = code) {
  href <- paste0("ide:run:pedant::", code)
  style_hyperlink(text, href)
}

defer <- function(expr, envir = parent.frame()) {
  call <- substitute(
    evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )
  do.call(on.exit, list(substitute(call), add = TRUE), envir = envir)
}