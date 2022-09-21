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
style_code <- function(code, use_pkgs = NULL, ignore_funs = NULL) {
  
  use_pkgs <- use_pkgs %||% guess_pkgs(code)
  
  out <- insert_dcs(code, use_pkgs, ignore_funs)
  
  set_last_style(list(code = out))
  
  last_style()
  
  out$styled
  
}



