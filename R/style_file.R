style_files <- function(files = list.filess(c(".", "R"), pattern = "\\.R$", full.names = TRUE),
                        packages = current_packages(),
                        ignore = imported_functions()) {
  
  names(files) <- files
  
  files <- map(files, read_file)
  files <- map(files, style_code)
  
  map(files, write_file)
  
  invisible(files)
  
}