# #' Shiny server factory
# #'
# #' @return shiny server
# #' @export
# server_factory <- function() {
#   function(input, output, session) {
#     require(dplyr)
#     require(ggplot2)
#     require(tidyr)
#     require(cognitive.index.lookup)
#     # source("https://raw.githubusercontent.com/agdamsbo/ENIGMAtrial_R/main/src/plot_index.R")
#     # source("https://raw.githubusercontent.com/agdamsbo/ENIGMAtrial_R/main/src/index_from_raw.R")
# 
#     dat <- reactive({
#       df <- data.frame(
#         "id" = "1",
#         "ab" = input$ab,
#         "age" = input$age,
#         "imm" = input$rs1,
#         "vis" = input$rs2,
#         "ver" = input$rs3,
#         "att" = input$rs4,
#         "del" = input$rs5,
#         stringsAsFactors = FALSE
#       )
#       return(df)
#     })
# 
#     dat_u <- reactive({
#       # input$file1 will be NULL initially. After the user selects
#       # and uploads a file, head of that data file by default,
#       # or all rows if selected, will be shown.
# 
# 
#       req(input$file1)
# 
#       df <- read.csv(input$file1$datapath,
#         header = input$header,
#         sep = input$sep,
#         quote = input$quote
#       )
# 
#       return(df)
#     })
# 
#     dat_f <- reactive({
#       if (input$type == 1) {
#         dat()
#       } else if (input$type == 2) {
#         dat_u()
#       }
#     })
# 
#     index_p <- reactive({
#       index_from_raw(
#         ds = dat_f(),
#         indx = index_table,
#         version.col = "ab",
#         age.col = "age",
#         raw_columns = c("imm", "vis", "ver", "att", "del")
#       )
#     })
# 
# 
#     output$ndx.tbl <- renderTable({
#       index_p() |>
#         select("id", "ab", contains("_is")) |>
#         setNames(c("ID", "ab", "imm", "vis", "ver", "att", "del", "Total"))
#     })
# 
#     output$per.tbl <- renderTable({
#       index_p() |>
#         select("id", "ab", contains("_per")) |>
#         setNames(c("ID", "ab", "imm", "vis", "ver", "att", "del", "Total"))
#     })
# 
# 
#     output$ndx.plt <- renderPlot({
#       plot_index(index_p(), sub_plot = "_is", facet.by = "ab")
#     })
# 
#     output$per.plt <- renderPlot({
#       plot_index(index_p(), sub_plot = "_per", facet.by = "ab")
#     })
# 
#     # Downloadable csv of selected dataset ----
#     output$downloadData <- downloadHandler(
#       filename = "index_lookup.csv",
#       content = function(file) {
#         write.csv(index_p(), file, row.names = FALSE)
#       }
#     )
#   }
# }
# 
# 
# 
# #' UI factory for shiny app
# #'
# #' @return shiny ui
# #' @export
# ui_factory <- function() {
#   require(shiny)
#   # require(ggplot2)
# 
#   fluidPage(
# 
#     ## -----------------------------------------------------------------------------
#     ## Application title
#     ## -----------------------------------------------------------------------------
#     titlePanel("Calculating cognitive index scores in multidimensional testing.",
#       windowTitle = "Cognitive test index calculator"
#     ),
#     h5("Please note this calculator is only meant as a proof of concept for educational purposes,
#      and the author will take no responsibility for the results of the calculator.
#      Uploaded data is not kept, but please, do not upload any sensitive data."),
# 
#     ## -----------------------------------------------------------------------------
#     ## Side panel
#     ## -----------------------------------------------------------------------------
# 
#     sidebarPanel(
#       h4("Test resultsData format"),
#       radioButtons(
#         inputId = "type",
#         label = "Data type",
#         inline = FALSE,
#         choiceNames = c(
#           "Single entry",
#           "File upload"
#         ),
#         choiceValues = c(1, 2)
#       ),
# 
#       # Horizontal line ----
#       tags$hr(),
# 
#       ## -----------------------------------------------------------------------------
#       ## Single entry
#       ## -----------------------------------------------------------------------------
# 
#       conditionalPanel(
#         condition = "input.type==1",
#         numericInput(
#           inputId = "age",
#           label = "Age",
#           value = 60
#         ),
#         radioButtons(
#           inputId = "ab",
#           label = "Test version (A/B)",
#           inline = FALSE,
#           choiceNames = c(
#             "A",
#             "B"
#           ),
#           choiceValues = c("1", "2")
#         ),
#         numericInput(
#           inputId = "rs1",
#           label = "Immediate memory",
#           value = 35
#         ),
#         numericInput(
#           inputId = "rs2",
#           label = "Visuospatial functions",
#           value = 35
#         ),
#         numericInput(
#           inputId = "rs3",
#           label = "Verbal functions",
#           value = 30
#         ),
#         numericInput(
#           inputId = "rs4",
#           label = "Attention",
#           value = 35
#         ),
#         numericInput(
#           inputId = "rs5",
#           label = "Delayed memory",
#           value = 40
#         )
#       ),
# 
#       ## -----------------------------------------------------------------------------
#       ## File upload
#       ## -----------------------------------------------------------------------------
# 
#       conditionalPanel(
#         condition = "input.type==2",
# 
#         # Input: Select a file ----
# 
#         fileInput("file1", "Choose CSV File",
#           multiple = FALSE,
#           accept = c(
#             "text/csv",
#             "text/comma-separated-values,text/plain",
#             ".csv"
#           )
#         ),
#         h6("Columns: id, ab, age, imm, vis, ver, att, del."),
# 
#         # Horizontal line ----
#         tags$hr(),
# 
#         # Input: Checkbox if file has header ----
#         checkboxInput("header", "Header", TRUE),
# 
#         # Input: Select separator ----
#         radioButtons("sep", "Separator",
#           choices = c(
#             Comma = ",",
#             Semicolon = ";",
#             Tab = "\t"
#           ),
#           selected = ","
#         ),
# 
#         # Input: Select quotes ----
#         radioButtons("quote", "Quote",
#           choices = c(
#             None = "",
#             "Double Quote" = '"',
#             "Single Quote" = "'"
#           ),
#           selected = '"'
#         ),
#       ),
# 
# 
#       ## -----------------------------------------------------------------------------
#       ## Download output
#       ## -----------------------------------------------------------------------------
# 
#       # Horizontal line ----
#       tags$hr(),
#       h4("Download results"),
# 
#       # Button
#       downloadButton("downloadData", "Download")
#     ),
#     mainPanel(
#       tabsetPanel(
# 
#         ## -----------------------------------------------------------------------------
#         ## Summary tab
#         ## -----------------------------------------------------------------------------
#         tabPanel(
#           "Summary",
#           h3("Index Scores"),
#           htmlOutput("ndx.tbl", container = span),
#           h3("Percentiles"),
#           htmlOutput("per.tbl", container = span)
#         ),
# 
#         ## -----------------------------------------------------------------------------
#         ## Plots tab
#         ## -----------------------------------------------------------------------------
# 
#         tabPanel(
#           "Plots",
#           h3("Index Scores"),
#           plotOutput("ndx.plt"),
#           h3("Percentiles"),
#           plotOutput("per.plt")
#         )
#       )
#     )
#   )
# }

#' Launch the included Shiny-app for index calculations
#'
#' @return shiny app
#' @export
shiny_index <- function() {
  shiny::runApp(appDir = here::here("app/"),launch.browser = TRUE)

  # shiny::shinyApp(
  #   ui_factory(),
  #   server_factory()
  # )
}
