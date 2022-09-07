subset_style_data <- function(x, file = NULL, package = NULL, fun = NULL) {
  
  if (!is.null(file) && !isTRUE(file)) {
    
    files      <- names(x)
    x          <- subset_by_file(x, file)
    bad_inputs <- setdiff(file, files)
    n_bad      <- length(bad_inputs)
    
    if (length(x) == 0) {
      cli_abort(
        c("Unrecognised {qty(n_bad)} file{?s} {.file {bad_inputs}}",
          i = "Styled files are {.file {unique(files)}}"),
        call = caller_env()
      )
    }
    
    if (length(bad_inputs) > 0) {
      cli_warn(
        "Unrecognised {qty(n_bad)} file{?s} {.file {bad_inputs}}",
        call = caller_env()
      )
    }
    
  }
  
  if (!is.null(package) && !isTRUE(package)) {
    
    files      <- names(x)
    pkgs       <- unique(unlist(map(x, ~ .x$parse_data$package)))
    x          <- subset_by_pkg(x, package)
    bad_inputs <- setdiff(package, pkgs)
    n_bad      <- length(bad_inputs)
    
    if (length(x) == 0) {
      cli_abort(
        c("Unrecognised {qty(n_bad)} package{?s} {.pkg {bad_inputs}}",
          i = "Calls were only inserted for functions from {.pkg {pkgs}}",
          i = if (!is.null(file)) {
            "Looking in {qty(length(files))} file{?s} {.file {files}}"
          }),
        call = caller_env()
      )
    }
    
  }
  
  if (!is.null(fun) && !isTRUE(fun)) {
    
    files      <- names(x)
    pkgs       <- unique(unlist(map(x, ~ .x$parse_data$package)))
    funs       <- unique(unlist(map(x, ~ .x$parse_data$text)))
    x          <- subset_by_fun(x, fun)
    bad_inputs <- setdiff(fun, funs)
    n_bad      <- length(bad_inputs)
    
    if (length(x) == 0) {
      cli_abort(
        c("No calls were styled to {qty(n_bad)} function{?s} {.fun {bad_inputs}}",
          i = "Styled {qty(length(funs))} call{?s} were {.fun {funs}}",
          i = if (!is.null(package)) {
            "Looking for calls inserted from {qty(length(pkgs))} package{?s} {.pkg {pkgs}}"
          },
          i = if (!is.null(file)) {
            "Looking in {qty(length(files))} file{?s} {.file {files}}"
          }),
        call = caller_env()
      )
    }
    
  }
  
  x
  
}

subset_by_file <- function(x, file) {
  keep(x, ~ (.x$src %||% "code") %in% file)
}

subset_by_pkg <- function(x, pkg) {
  x <- map(x, function(xi) {
    xi$parse_data <- xi$parse_data[xi$parse_data$package %in% pkg, ] 
    xi
  })
  keep(x, ~ nrow(.x$parse_data) > 0)
}

subset_by_fun <- function(x, fun) {
  x <- map(x, function(xi) {
    xi$parse_data <- xi$parse_data[xi$parse_data$text %in% fun, ]
    xi
  })
  keep(x, ~ nrow(.x$parse_data) > 0)
}