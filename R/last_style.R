

#' @export
last_style <- function(x = NULL, 
                       file = NULL, 
                       pkg = NULL,
                       fun = NULL,
                       reset = TRUE) {
  
  x <- x %||% cur_print_data() %or% last_style_data()
  
  if (exit_if_no_styles_yet(x)) {
    return(invisible(x))
  }
  
  if (reset) set_cur_print(x)
  
  x_small <- subset_style_data(x, file = file, pkg = pkg, fun = fun)
  
  if (!is.null(fun)) {
    last_styled_lines(x_small, fun = TRUE)
    return(invisible(x))
  }
  
  if (!is.null(pkg)) {
    last_styled_tokens(x_small, pkg = TRUE)
    return(invisible(x))
  }
  
  last_styled_files(x_small)
  invisible(x)
  
}

# Prints files to pkg detail
#' @export
last_styled_files <- function(x = last_style_data(), file = NULL, pkg = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) {
    return(invisible(x))
  }
  
  x_small <- subset_style_data(x, file = file, pkg = pkg, fun = fun)
  
  imap(x, function(x, file) {
    
    restyles   <- count(x$parse_data, "pkg")
    restyles   <- restyles[order(-restyles$n), , drop = FALSE]
    restyles$n <- comma(restyles$n)
    restyles <- transform(
      restyles,
      pkg = style_run(
        sprintf('last_style(file = "%s", pkg = "%s", reset = FALSE)', file, pkg),
        text = pkg
      )
    )
    
    file_view_pkgs <- style_run(
      sprintf('last_style(file = "%s", pkg = TRUE)', file), 
      text = file
    )
    
    cli_h2("Restyles in {.file {file_view_pkgs}}")
    map2(
      restyles$n, restyles$pkg,
      ~ cli_bullets(c("*" = "{.strong {.x}} from {.pkg {.y}}"))
    )
    
    NULL
    
  })
  
  cat_line()
  
  cli_bullets(c(
    ">" = style_maybe_run(
      'last_style(fun = TRUE, reset = FALSE)',
      "view all changes"
    )
  ))
  
  invisible(x)
  
}

# Prints pkgs to function detail
#' @export
last_styled_tokens <- function(x = last_style_data(), file = NULL, pkg = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) {
    return(invisible(x))
  }
  
  x_small <- subset_style_data(x, file = file, pkg = pkg, fun = fun)
  
  imap(x, function(x, file) {
    
    restyles   <- count(x$parse_data, "pkg", "text", "new_text")
    restyles   <- restyles[order(restyles$pkg, -restyles$n), , drop = FALSE]
    restyles$n <- comma(restyles$n)
    restyles <- transform(
      restyles,
      new_text = style_run(
        sprintf(
          'last_style(file = "%s", pkg = "%s", fun = "%s", reset = FALSE)', 
          file, pkg, text
        ),
        text = new_text
      )
    )
    
    file <- style_run(
      sprintf('last_style(file = "%s", pkg = TRUE)', file), 
      text = file
    )
    
    extra_info <- if (!is.null(pkg)) {
      format_inline("to functions from {.pkg {unique(restyles$pkg)}}")
    } 
    
    cli_h2("Restyles in {.file {file}} {extra_info}")
    map2(
      restyles$n, restyles$new_text,
      ~ cli_bullets(c("*" = "{.strong {.x}} to {.fun {.y}}"))
    )
    
    NULL
  })
  
}

#' @export
last_styled_lines <- function(x = last_style_data(), file = NULL, pkg = NULL, fun = NULL) {
  
  if (exit_if_no_styles_yet(x)) {
    return(invisible(x))
  }
  
  x_small <- subset_style_data(x, file = file, pkg = pkg, fun = fun)
  
  imap(x, function(x, file) {
    
    restyles   <- x$parse_data
    old_code   <- strsplit(x$original, "\n")[[1]]
    new_code   <- strsplit(x$styled, "\n")[[1]]
    
    tokens <- if (isTRUE(fun)) {
      insertions <- sort(unique(restyles$new_text))
      format_inline("to {.fun {insertions}}") 
    }
    
    cli_h2("Restyles in {.file {file}} {tokens}")
    
    for (line_n in unique(restyles$line1)) {
      
      lines <- subset(restyles, line1 == line_n)
      
      old <- old_code[line_n]
      new <- new_code[line_n]
      
      fmt_old <- col_red
      fmt_new <- col_green
      
      for (r in seq_len(nrow(lines))) {
        row <- lines[r, , drop = FALSE]
        old <- replace_substr(old, row$col1, row$col2, fmt_old)
        new <- replace_substr(new, row$new_col1, row$new_col2, fmt_new)
        
        # Correct cols for extra chars added by formatting
        lines <- transform(
          lines,
          col1     = col1     + nchar(fmt_old("")),
          col2     = col2     + nchar(fmt_old("")),
          new_col1 = new_col1 + nchar(fmt_new("")),
          new_col2 = new_col2 + nchar(fmt_new(""))
        )
      }
      
      cli_h3(style_open_file(
        file, paste("Line", comma(line_n)), 
        line = line_n, col = row$new_col1
      ))
      
      cli_bullets(c(
        "*" = "Old: {.code {old}}",
        "*" = "New: {.code {new}}"
      ))
      
    }
    
    NULL
    
  })
  
}

exit_if_no_styles_yet <- function(x) {
  if (length(x) == 0) {
    cat_line("Styling hasn't been run yet!")
    TRUE
  }
  FALSE
}

new_style_info <- function(x) {
  if (is_installed("tibble")) {
    x <- map(x, map_if, is.data.frame, tibble::as_tibble)
  }
  class(x) <- c("pedant_style_info", class(x))
  x
}

# last_style_env ---------------------------------------------------------
last_style_env <- new.env()

last_style_data <- function() {
  new_style_info(as.list(last_style_env))
}

clear_last_style <- function() {
  rm(list = ls(envir = last_style_env), envir = last_style_env)
  invisible(NULL)
}

set_last_style <- function(x) {
  clear_last_style()
  list2env(x, last_style_env)
  invisible(NULL)
}

# cur_print_env ---------------------------------------------------------
cur_print_env <- new.env()

cur_print_data <- function() {
  new_style_info(as.list(cur_print_env))
}

clear_cur_print <- function() {
  rm(list = ls(envir = cur_print_env), envir = cur_print_env)
  invisible(NULL)
}

set_cur_print <- function(x) {
  clear_cur_print()
  list2env(x, cur_print_env)
  invisible(NULL)
}
