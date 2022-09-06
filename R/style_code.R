#' Make function calls explicit
#'
#' This function takes a block of code and seeks to make all function calls
#' 'explicit' through the use of the double-colon operator `::`. This function
#' is bound to the RStudio addin `"Make function calls explicit"`. See examples
#' for usage.
#'
#' This function behaves differently depending on the context.
#' - **Not package development**: If the current
#' context is not package development, then it will make function calls explicit
#' using the currently attached packages (i.e. the ones attached by calls to
#' `library()`).
#' - **Package development**: If it detects that the current context is package
#' development it will make function calls explicit using packages in the
#' 'Imports' field of the package `DESCRIPTION`. If the package being developed
#' imports any packages in their entirety (i.e if `Import pkg` appears in the
#' `NAMESPACE` file), calls to functions from these packages will be left
#' unchanged.
#' See `current_packages()` for more information.
#' 
#' TODO: 
#' * figure out what to do with specials, e.g. %||%. Probs leave them alone
#'   but add a msg about it
#' * Add an 'undo' feature
#' * Better messages. Should be quite informative I think
#'
#' @param code Code to transform. Either a character vector or `NULL`, in which
#'   case any highlighted code (in RStudio) will be used.
#' @param packages A character vector of package names. The order is
#'   important here - see examples for details.
#' @param ignore Functions to ignore when applying the transformation
#'
#' @return The transformed `code` as a character string
#' @export
#'
#' @examples
#' code <- "
#'   cars <- as_tibble(mtcars)
#'   cars %>%
#'     filter(mpg > 20) %>%
#'     summarise(across(everything(), n_distinct))
#' "
#'
#' # Code will be transformed to use the double-colon operator, but notice
#' # that `n_distinct` is not transformed as it is not followed by `()`
#' cat(add_double_colons(code, "dplyr"))
#'
#' # You can specify functions that shouldn't be transformed:
#' cat(add_double_colons(code, "dplyr", ignore = "across"))
#'
#' # Beware namespace conflicts! The following are not the same, mimicking
#' # the effects of reordering calls to `library()`:
#' cat(add_double_colons(code, c("dplyr", "stats")))
#'
#' cat(add_double_colons(code, c("stats", "dplyr")))
style_code <- function(code, packages = NULL, ignore = NULL) {
  
  packages <- packages %||% guess_pkgs(code)
  
  # Difficulties using parse data:
  # * detect when functions have been defined in the current scope
  pd <- parse_code(code)
  
  # If a `::` token shares a parent with a `SYMBOL_FUNCTION_CALL` token, then
  # the function call is already explicit
  already_explicit <- pd$parent %in% pd$parent[pd$text == "::"]
  
  # 1. Filter for symbol function calls which aren't already explicit and
  #    are 'terminal' (otherwise they might appear more than once)
  # 2. Get the package which each function comes from
  # 3. Generate a new string to insert in place of the function call
  # 4. Get difference in text widths so replacements don't get out of whack
  # 5. Order to make 100% sure that calls will be made explicit in order of 
  #    their appearance. Important when adjusting `col1` and `col2` later
  pd <- subset(pd, token == "SYMBOL_FUNCTION_CALL" & !already_explicit & terminal)
  pd <- transform(pd, package = pkg_from_fun(fun = text, packages = packages))
  pd <- transform(pd, new_text = ifelse(is.na(package), text, paste0(package, "::", text)))
  pd <- transform(pd, width_diff = nchar(new_text) - nchar(text))
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  
  code <- strsplit(code, "\n")[[1]]
  
  for (r in seq_len(nrow(pd))) {
    
    # Replace fn() with pkg::fun()
    pd_row     <- pd[r, ]
    line       <- pd_row$line1 
    code[line] <- with(pd_row, replace_substr(code[line], col1, col2, new_text))
    
    # Adjust `col1` and `col2` to account for new number of chars
    pd <- transform(
      pd, 
      col1 = ifelse(line1 != pd_row$line1, col1, col1 + width_diff),
      col2 = ifelse(line1 != pd_row$line1, col2, col2 + width_diff)
    )
    
  }
  
  paste(code, collapse = "\n")
  
}


pkg_from_fun <- function(fun, packages = guess_pkgs()) {
  
  namespaces        <- lapply(packages, getNamespaceExports)
  names(namespaces) <- packages
  
  # remove possible backticks for lookup
  fun <- strip_quotes(fun, "`")
  
  vapply(
    fun, 
    FUN = function(f) {
      
      for (pkg in packages) {
        if (f %in% namespaces[[pkg]]) {
          return(pkg)
        }
      }
      
      NA_character_
      
    },
    FUN.VALUE = character(1)
  )
  
}


style_code2 <- function(code = NULL,
                        packages = guess_packages(code),
                        ignore = imported_functions()) {

  # Need to make this small adjustment to (very badly styled) code since
  # variable-length lookbehinds aren't possible
  code <- gsub(":: +", "::", code)

  # Regular expression to extract function calls
  backticks_fns <- "`[^`]+`(?= *[(])"
  syntactic_fns <- "(?<=[^a-zA-Z_]|^)[a-zA-Z._]+(?= *[(])"
  exclude_dcs   <- "(?<!::)"
  funs_regex    <- sprintf("%s(%s|%s)", exclude_dcs, backticks_fns, syntactic_fns)

  all_calls   <- str_extract_all(code, funs_regex)
  called_funs <- unique(all_calls)

  # Get a lookup list of names = packages, values = namespace exports
  pkg_lookup        <- lapply(packages, getNamespaceExports)
  names(pkg_lookup) <- packages

  # Helper to retrieve the `pkg::fun` text for a function `fun`
  get_pkg <- function(fun) {

    fun1 <- gsub("(^`)|(`$)", "", fun)

    for (pkg in packages) {
      if (fun1 %in% pkg_lookup[[pkg]]) {
        if (pkg == "base" || fun1 %in% ignore) {
          return(fun)
        } else {
          return(paste0(pkg, "::", fun))
        }
      }
    }
    
    NA_character_
  }

  # Get the replacement text for each function call
  called_funs_pkgs <- vapply(called_funs, get_pkg, character(1))
  no_pkg <- is.na(called_funs_pkgs)

  # Warn about any unfound functions
  if (any(no_pkg) > 0) {
    warning(sprintf(
      "Couldn't find packages exporting %d function(s): `%s()`",
      sum(no_pkg), paste(called_funs[no_pkg], collapse = "()`, `")
    ), call. = FALSE)

    called_funs_pkgs[no_pkg] <- names(called_funs_pkgs)[no_pkg]

  }

  # Get the full vector of replacements for the regex matches
  replacements <- called_funs_pkgs[
    vapply(all_calls, function(x) which(names(called_funs_pkgs) == x), integer(1))
  ]
  
  out <- str_replace_all(code, funs_regex, replacements)

  out

}

