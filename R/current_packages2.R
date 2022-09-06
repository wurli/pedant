#' - New behaviour: 
#'   - Should check a global option
#'   - Then any `library()` calls in selected code.
#'   - Then the `DESCRIPTION` file if present
#'   - Then use search path - but you should have to opt in to this because
#'     it can easily go wrong.
#'   - Should be much chattier
#'   
guess_pkgs <- function(code = NULL,
                       order = c("global_option",
                                 "package_desc",
                                 "library_calls")) {
  
  opts <- alist(
    global_option = pkgs_from_opt(),
    package_desc  = pkgs_from_desc(),
    library_calls = pkgs_from_library_calls(code),
    search_path   = pkgs_from_search_path()
  )
  
  opts <- opts[order]
  
  for (opt in names(opts)) {
    
    pkgs <- eval(opts[[opt]])
    
    if (!is.null(pkgs)) {
      cat("Getting packages from", opt)
      break
    }
    
  }
  
  pkgs
  
}

#' Get packages to use when inserting `::`
#'
#' * `pkgs_from_opt()` checks a global option
#' * `pkgs_from_library_calls()` checks code for calls to `library()`/`require()`
#' * `pgks_from_desc()` gets the packages listed in the current
#'   project's `DESCRIPTION` file, minus any packages which are noted in the 
#'   `NAMESPACE` file as being imported in their entirety
#' * `pkgs_from_search_path()` gets the packages which are currently loaded
#'
#' @param opt The name of the global option to check
#'
#' @return A character vector of package names or `NULL`
pkgs_from_opt <- function(opt = "pedant_packages") {
  getOption(opt)
}

#' @rdname pkgs_from_opt
#' 
#' @param code A character string containing code to search
#' @param funs Functions which indicate that a package is available
#'
#' @examples
#' code <- "library(dplyr)
#' library(`'tidyr'`)
#' library(\"ggplot2\")
#' require('`another_pkg`')
#' do |> some() |> stuff"
#' 
#' pkgs_from_library_calls(code)
pkgs_from_library_calls <- function(code, 
                                    funs = c(
                                      "library", 
                                      "require", 
                                      "requireNamespace"
                                    )) {
  
  if (length(code) == 0) {
    return(code)
  }
  
  code <- paste(code, collapse = "\n")
  
  regex <- sprintf("(?<=(%s)\\()[^)]+(?=\\))", paste(funs, collapse = "|"))
  loaded_pkgs <- str_extract_all(code, regex)
  
  unique(strip_quotes(loaded_pkgs))
  
}

#' @rdname pkgs_from_opt
#'
#' @param dir A directory in which to search a `DESCRIPTION` and `NAMESPACE` files
#' @param types A subset of `c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")`
pkgs_from_desc <- function(dir = ".", types = "Imports") {
  setdiff(get_dependencies(dir, types), get_imports(dir))
}

#' @rdname pkgs_from_opt
#' 
#' @param search_path A character vector as returned by `search()`
pkgs_from_search_path <- function(search_path = search()) {
  out <- search_path[grepl("^package:", search_path)]
  sub("^package:", "", out)
}

get_dependencies <- function(dir = ".",
                             types = c("Imports", "Depends", "Suggests",
                                       "Enhances", "LinkingTo")) {
  
  
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    return(NULL)
  }
  
  types <- match.arg(types, several.ok = TRUE)
  
  deps <- tryCatch(
    pkgload::pkg_desc(dir)$get_deps(),
    error = function(e) NULL
  )
  
  if (is.null(deps)) return(NULL)
  
  deps$package[deps$type %in% types]
  
}

get_imports <- function(dir = ".") {
  
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    return(NULL)
  }
  
  imports <- tryCatch(
    pkgload::parse_ns_file(dir)$imports,
    error = function(e) NULL
  )
  
  if (is.null(imports)) return(NULL)
  
  out <- list(
    packages  = map(imports, function(x) if (length(x) == 1) x else NULL),
    functions = map(imports, function(x) x[-1])
  )
  
  map(out, unlist, use.names = FALSE)$packages
  
}
