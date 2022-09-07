

#' @export
last_style <- function(x = last_style_data(), file = NULL, package = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) return(invisible(x))
  
  x_small <- subset_style_data(x, file = file, package = package, fun = fun)
  
  if (!is.null(fun)) {
    last_styled_lines(x_small)
    return(invisible(x))
  }
  
  if (!is.null(package)) {
    last_styled_tokens(x_small, package = package)
    return(invisible(x))
  }
  
  last_styled_files(x_small)
  invisible(x)
  
}

# Prints files to package detail
last_styled_files <- function(x = last_style_data(), file = NULL, package = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) return(invisible(x))
  
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
    
    cli_h2("Restyles in {.file {file}}")
    map2(
      restyles$n, restyles$package,
      ~ cli_bullets(c("*" = "{.strong {.x}} from {.pkg {.y}}"))
    )
    
    NULL
    
  })
  
  invisible(x)
  
}

# Prints packages to function detail
last_styled_tokens <- function(x = last_style_data(), package = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) return(invisible(x))
  
}

subset_style_data <- function(x, file = NULL, package = NULL, fun = NULL) {
  
  if (!is.null(file)) {
    
    files      <- names(x)
    x          <- keep(x, ~ (.x$src %||% "code") %in% file)
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
  
  if (!is.null(package)) {
    
    files      <- names(x)
    pkgs       <- unique(unlist(map(x, ~ .x$parse_data$package)))
    x          <- map(x, map_if, is.data.frame, ~ .x[.x$package %in% package, ])
    x          <- keep(x, ~ nrow(.x$parse_data) > 0)
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
  
  if (!is.null(fun)) {
    
    files      <- names(x)
    pkgs       <- unique(unlist(map(x, ~ .x$parse_data$package)))
    funs       <- unique(unlist(map(x, ~ .x$parse_data$text)))
    x          <- map(x, map_if, is.data.frame, ~ .x[.x$text %in% fun, ])
    x          <- keep(x, ~ nrow(.x$parse_data) > 0)
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

exit_if_no_styles_yet <- function(x) {
  if (length(x) == 0) {
    cat_line("Styling hasn't been run yet!")
    TRUE
  }
  FALSE
}

last_style_data <- function() {
  
  out <- as.list(last_style_env)
  
  if (is_installed("tibble")) {
    out <- map(out, map_if, is.data.frame, tibble::as_tibble)
  }
  
  class(out) <- c("pedant_style_info", class(out))
  
  out
  
}

# last_style_env ---------------------------------------------------------
last_style_env <- new.env()

clear_last_style <- function() {
  rm(list = ls(envir = last_style_env), envir = last_style_env)
  invisible(NULL)
}

set_last_style <- function(x) {
  clear_last_style()
  list2env(x, last_style_env)
  invisible(NULL)
}