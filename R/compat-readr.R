read_file <- function(x) {
  
  readr <- requireNamespace("readr", quietly = TRUE)
  
  if (readr) readr::read_file(x) else paste(readLines(x), collapse = "\n")
   
  x
  
}

write_file <- function(x, name) {
  
  readr <- requireNamespace("readr", quietly = TRUE)
  
  if (readr) readr::write_file(x, name) else writeLines(x, name)
  
  invisible(x)
  
}