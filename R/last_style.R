#' @export
last_style <- function(file = NULL, package = NULL) {
  
  out <- as.list(last_style_env)
  
  if (is_installed("tibble")) {
    
    out <- map(out, map_if, is.data.frame, tibble::as_tibble)
    
  }
  
  class(out) <- c("pedant_style_info", class(out))
  
  print(out, file, package)
  
  invisible(out)
  
}

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