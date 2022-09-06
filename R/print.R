print.pedant_style_info <- function(x, file = NULL, package = NULL, ...) {
  
  original <- x
  
  if (length(x) == 0) {
    cat_line("Styling hasn't been run yet!")
    return(invisible(x))
  }
  
  src <- names(x)
  
  if (!all(src == "code") && !is.null(file)) {
    
    if (!file %in% src) {
      cli_abort(c(
        "File {.file {file}} has not been styled",
        i = "Styled files are {.file {unique(src)}}"
      ))
    }
    
    x <- x[file]
    
  }
  
  if (length(x) == 1 || !is.null(package)) {
    imap(x, print_last_styled_file, package = package)
    return(invisible(original))
  }
  
  imap(x, function(x, file) {
    
    restyles   <- count(x$parse_data, "package")
    restyles   <- restyles[order(-restyles$n), , drop = FALSE]
    restyles$n <- comma(restyles$n)
    restyles <- transform(
      restyles,
      package = style_pedant_run(
        sprintf('last_style(file = "%s", package = "%s")', file, package),
        text = package
      )
    )
    
    file <- style_pedant_run(
      sprintf('last_style(file = "%s")', file), 
      text = file
    )
    
    cli_h1("Restyles in {.file {file}}")
    
    map2(
      restyles$n, restyles$package,
      ~ cli_bullets(c("*" = "{.strong {.x}} from {.pkg {.y}}"))
    )
    
    NULL
    
  })
  
  invisible(original)
  
}

print_last_styled_file <- function(x, name = NULL, package = NULL) {
  
  cat("yo")
  
  
  
}

# 1. all files:         just the number of packages inserted from each package
# 2. individual files:  the number of occurrences of each insertion
# 3. individual packages: contexts