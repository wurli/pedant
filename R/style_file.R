style_files <- function(files = list.filess(c(".", "R"), pattern = "\\.R$", full.names = TRUE),
                        packages = current_packages(),
                        ignore = imported_functions()) {
  
  names(files) <- files
  
  files <- lapply(files, read_file)
  files <- lapply(files, style_code)
  
  lapply(files, write_file)
  
  invisible(files)
  
}