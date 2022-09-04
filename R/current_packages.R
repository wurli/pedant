#' Get packages from the current context
#'
#' These functions find the packages/functions to use when running
#' `add_double_colons()`.
#'
#' - New behaviour: 
#'   - Should check a global option
#'   - Then any `library()` calls
#'   - Then the `DESCRIPTION` file
#'   - Then use search path - but you should have to opt in to this because
#'     it can easily go wrong.
#'   - Should be much chattier
#'
#' - `current_packages()` first checks if the current context is package
#' development. If it is, then it returns the packages which are listed in the
#' package `DESCRIPTION` as dependencies, but will not return any packages also
#' listed as imports in the package `NAMESPACE`. If the current context is not
#' package development, the currently attached packages (as given by `search()`)
#' are used. Note that if `{pkgload}` is not installed then the latter option is
#' always used.
#' - `imported_functions()` looks for a package `NAMESPACE` file and returns the
#' names of all imported functions. If a `NAMESPACE` file is not found, or if
#' `{pkgload}` is not loaded, `NULL` is returned.
#'
#' @param dir The current working directory
#' @param base_packages Default packages to include
#' @param dev_context Is the current context package development?
#' @param include_types The types of package imports to return if the current
#'   context is package development. Should be a subset of
#'   `c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")`
#'
#' @export
current_packages <- function(dir = ".",
                             base_packages = getOption("defaultPackages"),
                             dev_context = is_dev_context(),
                             include_types = "Imports") {
  
  out <- if (dev_context) {
    dev_context_pkgs(dir, include_types)
  } else {
    loaded_packages()
  }
  
  unique(c(out, base_packages, "base"))
  
}

#' @rdname current_packages
#' @export
imported_functions <- function(dir = ".") {
  get_imports(dir)$functions
}


#' @rdname current_packages
#' @export
is_dev_context <- function(dir = ".") {
  if (!requireNamespace("pkgload", quietly = TRUE)) return(FALSE)
  tryCatch(
    {
      pkgload::pkg_name(dir)
      TRUE
    },
    error = function(e) FALSE
  )
}

imported_packages <- function(dir = ".") {
  get_imports(dir)$packages
}

dev_context_pkgs <- function(dir = ".", types = "Imports") {
  setdiff(get_dependencies(dir, types), imported_packages(dir))
}

loaded_packages <- function() {
  search_path <- search()
  out <- search_path[grepl("^package:", search_path)]
  sub("^package:", "", out)
}

