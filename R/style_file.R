style_files <- function(files = list.filess(c(".", "R"), pattern = "\\.R$", full.names = TRUE),
                        use_pkgs = guess_pkgs(),
                        ignore_funs = imported_funs(),
                        output_names = files) {
  
  if (is_formula(output_names)) {
    output_names <- as_function(output_names)
  }
  
  if (is_function(output_names)) {
    output_names <- output_names(files)
  }
  
  names(files) <- output_names
  
  files <- map(files, read_file)
  files <- map(files, style_code, use_pkgs = use_pkgs, ignore_funs = ignore_funs)
  
  imap(files, write_file)
  
  out <- map(files, ~ .x$styled)
  
  invisible(out)
  
}