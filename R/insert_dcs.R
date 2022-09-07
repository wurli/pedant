#' Title
#'
#' @param code 
#' @param use_pkgs 
#' @param ignore_funs 
#' @param src 
#'
#' @return
#' @export
#'
#' @examples
insert_dcs <- function(code, use_pkgs = NULL, ignore_funs = NULL, src = NULL) {
  
  original <- code
  
  use_pkgs <- use_pkgs %||% guess_pkgs(code)
  
  # Difficulties using parse data:
  # * detect when functions have been defined in the current scope
  pd <- parse_code(code)
  
  # If a `::` token shares a parent with a `SYMBOL_FUNCTION_CALL` token, then
  # the function call is already explicit
  already_explicit <- pd$parent %in% pd$parent[pd$text == "::"]
  
  tokens <- c("SYMBOL_FUNCTION_CALL")#, "SPECIAL")
  
  # 1. Filter for symbol function calls which aren't already explicit and
  #    are 'terminal' (otherwise they might appear more than once)
  # 2. Get the package which each function comes from
  # 3. Generate a new string to insert in place of the function call
  # 4. Get difference in text widths so replacements don't get out of whack
  # 5. Order to make 100% sure that calls will be made explicit in order of 
  #    their appearance. Important when adjusting `col1` and `col2` later
  pd <- subset(pd, token %in% tokens & !already_explicit & terminal)
  pd <- transform(pd, pkg = pkg_from_fun(fun = text, pkgs = use_pkgs))
  pd <- subset(pd, !is.na(pkg))
  pd <- transform(pd, new_text = paste0(pkg, "::", text))
  pd <- transform(pd, width_diff = nchar(new_text) - nchar(text))
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  pd <- transform(pd, new_col1 = col1, new_col2 = col2)
  
  pd$shifts <- 0
  
  code <- strsplit(code, "\n")[[1]]
  
  for (r in seq_len(nrow(pd))) {
    
    # Replace fn() with pkg::fun()
    pd_row     <- pd[r, ]
    line       <- pd_row$line1 
    code[line] <- with(pd_row, replace_substr(code[line], new_col1, new_col2, new_text))
    
    shift_col <- pd$line1 == pd_row$line1 & pd$new_col1 >= pd_row$new_col1
    
    # Adjust `col1` and `col2` to account for new number of chars
    pd <- transform(
      pd, 
      new_col1 = ifelse(shift_col, new_col1 + width_diff, new_col1),
      new_col2 = ifelse(shift_col, new_col2 + width_diff, new_col2),
      shifts = shifts + shift_col
    )
    
  }
  
  styled <- paste(code, collapse = "\n")
  
  list(
    original = glue::as_glue(original),
    styled = glue::as_glue(styled),
    parse_data = pd,
    src = src
  )
  
}


pkg_from_fun <- function(fun, pkgs = guess_pkgs()) {
  
  namespaces        <- map(pkgs, getNamespaceExports)
  names(namespaces) <- pkgs
  
  # remove possible backticks for lookup
  fun <- strip_quotes(fun, "`")
  
  map_chr(fun, function(f) {
    
    for (pkg in pkgs) {
      if (f %in% namespaces[[pkg]]) {
        return(pkg)
      }
    }
    
    NA_character_
    
  })
  
}



# insert_dcs2 <- function(code = NULL,
#                         pkgs = guess_pkgs(code),
#                         ignore = imported_functions()) {
#   
#   # Need to make this small adjustment to (very badly styled) code since
#   # variable-length lookbehinds aren't possible
#   code <- gsub(":: +", "::", code)
#   
#   # Regular expression to extract function calls
#   backticks_fns <- "`[^`]+`(?= *[(])"
#   syntactic_fns <- "(?<=[^a-zA-Z_]|^)[a-zA-Z._]+(?= *[(])"
#   exclude_dcs   <- "(?<!::)"
#   funs_regex    <- sprintf("%s(%s|%s)", exclude_dcs, backticks_fns, syntactic_fns)
#   
#   all_calls   <- str_extract_all(code, funs_regex)
#   called_funs <- unique(all_calls)
#   
#   # Get a lookup list of names = packages, values = namespace exports
#   pkg_lookup        <- map(pkgs, getNamespaceExports)
#   names(pkg_lookup) <- pkgs
#   
#   # Helper to retrieve the `pkg::fun` text for a function `fun`
#   get_pkg <- function(fun) {
#     
#     fun1 <- gsub("(^`)|(`$)", "", fun)
#     
#     for (pkg in pkgs) {
#       if (fun1 %in% pkg_lookup[[pkg]]) {
#         if (pkg == "base" || fun1 %in% ignore) {
#           return(fun)
#         } else {
#           return(paste0(pkg, "::", fun))
#         }
#       }
#     }
#     
#     NA_character_
#   }
#   
#   # Get the replacement text for each function call
#   called_funs_pkgs <- map_chr(called_funs, get_pkg)
#   no_pkg <- is.na(called_funs_pkgs)
#   
#   # Warn about any unfound functions
#   if (any(no_pkg) > 0) {
#     warning(sprintf(
#       "Couldn't find packages exporting %d function(s): `%s()`",
#       sum(no_pkg), paste(called_funs[no_pkg], collapse = "()`, `")
#     ), call. = FALSE)
#     
#     called_funs_pkgs[no_pkg] <- names(called_funs_pkgs)[no_pkg]
#     
#   }
#   
#   # Get the full vector of replacements for the regex matches
#   replacements <- called_funs_pkgs[
#     map_int(all_calls, function(x) which(names(called_funs_pkgs) == x))
#   ]
#   
#   out <- str_replace_all(code, funs_regex, replacements)
#   
#   out
#   
# }
