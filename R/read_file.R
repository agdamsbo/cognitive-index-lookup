
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
#'
#' @return tibble
#' @export
read_input <- function(file){
  
  ext <- file_extension(file)
  
  tryCatch(
    {
      if (ext == "csv") {
        df <- readr::read_csv(file,na.strings = c("NA", '""',""))
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file,na.strings = c("NA", '""',""))
      } else {
        stop("Input file format has to be either '.csv', '.xls' or '.xlsx'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  df
}