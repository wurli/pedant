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
    packages  = lapply(imports, function(x) if (length(x) == 1) x else NULL),
    functions = lapply(imports, function(x) x[-1])
  )

  lapply(out, unlist, use.names = FALSE)

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

#' @rdname current_packages
#' @export
imported_functions <- function(dir = ".") {
  get_imports(dir)$functions
}

#' Get packages from the current context
#'
#' These functions find the packages/functions to use when running
#' `add_double_colons()`.
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
#' @param include_types The types of package imports to return if the current
#'   context is package development. Should be a subset of
#'   `c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")`
#'
#' @export
current_packages <- function(dir = ".",
                             base_packages = c("base", getOption("defaultPackages")),
                             include_types = "Imports") {

  out <- if (is_dev_context()) {
    dev_context_pkgs(dir, include_types)
  } else {
    loaded_packages()
  }

  unique(c(out, base_packages))

}
