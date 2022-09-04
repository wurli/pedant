
style_selection <- function(pkgs_from = c("global_option",
                                          "package_desc",
                                          "library_calls")) {
  
  code <- selected_code()
  
  packages <- guess_pkgs(pkgs_from) # Should also return which functions not to style
  
  styled_code <- style_code(code)
  
  rstudioapi::insertText(styled_code)
  
  invisible(styled_code)
  
}

selected_code <- function(strict = TRUE) {
  
  exit <- if (strict) {
    function(x) stop(x, call. = FALSE)
  } else {
    function(x) on.exit(do.call("return", list(NULL), envir = parent.frame()))
  }
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    exit("{rstudioapi} must be installed")
  }
  
  if (!rstudioapi::isAvailable()) {
    exit("RStudio is not running")
  }
  
  code <- rstudioapi::selectionGet()$value
  
  if (identical(code, "")) {
    exit("No code selected!")
  }
  
  code
  
}