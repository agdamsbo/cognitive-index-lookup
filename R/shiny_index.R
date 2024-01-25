
#' Launch the included Shiny-app for index calculations
#'
#' @return
#' @export
#'
#' @examples
#' shiny_index()
shiny_index <- function(){
  shiny::runApp(appDir = here::here("shiny/"),launch.browser = TRUE)
}
