#' Launch the included Shiny-app for index calculations
#'
#' @return shiny app
#' @export
shiny_index <- function() {
  shiny::runApp(appDir = here::here("app/"), launch.browser = TRUE)
}
