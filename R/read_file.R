#' Helper to import files correctly
#'
#' @param filenames file names
#'
#' @return character vector
#' @export
#'
#' @examples
#' file_extension(list.files(here::here(""))[[2]])[[1]]
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- file_extension(file)

  tryCatch(
    {
      if (ext == "csv") {
        df <- readr::read_csv(file, na = consider.na)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file, na.strings = consider.na)
      } else {
        stop("Input file format has to be either '.csv', '.xls' or '.xlsx'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(shiny::safeError(e))
    }
  )

  df
}
