#' Format File with Error Handling
#'
#' This function reads the content of a file at the specified path,
#' makes R function calls explicit using the `pedant::add_double_colons` function,
#' and then writes the updated content back to the file.
#' If the file does not exist, it writes 'Error_002' to the specified path.
#' If an error occurs during the reading or processing of the file,
#' it writes 'Error_001' to the specified path.
#'
#' @param path A character string specifying the path to the file with
#' code to be formatted.
#'
#' @return None

ic <- function(path) {
  if (!file.exists(path)) {
    # If file does not exist, write an error message to the path
    writeLines("Error_002", con = path)
  } else {
    tryCatch(
      {
        # Attempt to read the file, process it, and write the updated content
        content <- readLines(path, encoding = "UTF-8")
        updated_content <- pedant::add_double_colons(content)
        writeLines(enc2utf8(updated_content), con = path)
      },
      error = function(e) {
        # If an error occurs during the process, write an error message
        writeLines("Error_001", con = path)
      }
    )
  }
}
