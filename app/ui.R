require(shiny)
require(bslib)
# require(ggplot2)
# source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/index_from_raw.R")
# source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/plot_index.R")
# source(here::here("R/index_from_raw.R"))
# source(here::here("R/plot_index.R"))

# ui <- fluidPage(

cards <- list(
  bslib::card(max_height = "200px",
    full_screen = TRUE,
    bslib::card_header("Index Scores table"),
    shiny::uiOutput("ndx.tbl")
  ),
  bslib::card(max_height = "200px",
    full_screen = TRUE,
    bslib::card_header("Percentiles table"),
    shiny::uiOutput("per.tbl")
  ),
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Index Scores plot"),
    shiny::plotOutput("ndx.plt")
  ),
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Percentiles plot"),
    shiny::plotOutput("per.plt")
  )
)


ui <- bslib::page_navbar(
  theme = bslib::bs_theme(bootswatch = "minty"),
  title = "Calculating cognitive index scores in multidimensional testing.",
  window_title = "Cognitive test index calculator",
  header=h6("Please note this calculator is only meant as a proof of concept for educational purposes,
     and the author will take no responsibility for the results of the calculator.
     Uploaded data is not kept, but please, do not upload any sensitive data."),

  ## -----------------------------------------------------------------------------
  ## Application title
  ## -----------------------------------------------------------------------------
  # titlePanel("Calculating cognitive index scores in multidimensional testing.",
  #   windowTitle = "Cognitive test index calculator"
  # ),
  

  ## -----------------------------------------------------------------------------
  ## Side panel
  ## -----------------------------------------------------------------------------

  # sidebarPanel(
  sidebar = bslib::sidebar(
    open = "open",
    h4("Test results"),
    radioButtons(
      inputId = "type",
      label = "Data type",
      inline = FALSE,
      choiceNames = c(
        "Single entry",
        "File upload"
      ),
      choiceValues = c(1, 2)
    ),

    # Horizontal line ----
    tags$hr(),

    ## -----------------------------------------------------------------------------
    ## Single entry
    ## -----------------------------------------------------------------------------

    conditionalPanel(
      condition = "input.type==1",
      helpText(em("Specify relevant data below, download results at the bottom")),
      numericInput(
        inputId = "age",
        label = "Age",
        value = 60
      ),
      radioButtons(
        inputId = "ab",
        label = "Test version (A/B)",
        inline = FALSE,
        choiceNames = c(
          "A",
          "B"
        ),
        choiceValues = c("1", "2")
      ),
      numericInput(
        inputId = "rs1",
        label = "Immediate memory",
        value = 48
      ),
      numericInput(
        inputId = "rs2",
        label = "Visuospatial functions",
        value = 37
      ),
      numericInput(
        inputId = "rs3",
        label = "Verbal functions",
        value = 29
      ),
      numericInput(
        inputId = "rs4",
        label = "Attention",
        value = 52
      ),
      numericInput(
        inputId = "rs5",
        label = "Delayed memory",
        value = 50
      )
    ),

    ## -----------------------------------------------------------------------------
    ## File upload
    ## -----------------------------------------------------------------------------

    conditionalPanel(
      condition = "input.type==2",

      # Input: Select a file ----

      fileInput(
        inputId = "file1",
        label = "Choose CSV File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          ".xlsx",
          ".xls"
        )
      ),
      helpText(em("Please specify relevant columns from your data, and press 'Load data'")),
      uiOutput("id_sel"),
      uiOutput("ab_sel"),
      uiOutput("age_sel"),
      uiOutput("imm_sel"),
      uiOutput("vis_sel"),
      uiOutput("ver_sel"),
      uiOutput("att_sel"),
      uiOutput("del_sel"),
      actionButton("load", "Load data", class = "btn-primary")
      # Horizontal line ----
      # tags$hr(),

      # Input: Checkbox if file has header ----
      # checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      # radioButtons("sep", "Separator",
      #              choices = c(
      #                Comma = ",",
      #                Semicolon = ";",
      #                Tab = "\t"
      #              ),
      #              selected = ","
      # ),

      # Input: Select quotes ----
      # radioButtons("quote", "Quote",
      #              choices = c(
      #                None = "",
      #                "Double Quote" = '"',
      #                "Single Quote" = "'"
      #              ),
      #              selected = '"'
      # ),
    ),
    # actionButton("update", "Update output", class = "btn-primary"),

    # Have tried to dunamically subset. Was working, then not. Head scratches..

    ## -----------------------------------------------------------------------------
    ## Download output
    ## -----------------------------------------------------------------------------

    # Horizontal line ----
    tags$hr(),
    h4("Download results"),

    # Button
    downloadButton("downloadData", "Download")
  ),
  layout_columns(
    cards[[1]], cards[[2]]
  ),
  layout_columns(
    cards[[3]], cards[[4]]
  )
  

  # mainPanel(
  #   tabsetPanel(
  #
  #     ## -----------------------------------------------------------------------------
  #     ## Summary tab
  #     ## -----------------------------------------------------------------------------
  #     tabPanel(
  #       "Summary",
  #       h3("Index Scores"),
  #       htmlOutput("ndx.tbl", container = span),
  #       h3("Percentiles"),
  #       htmlOutput("per.tbl", container = span)
  #     ),
  #
  #     ## -----------------------------------------------------------------------------
  #     ## Plots tab
  #     ## -----------------------------------------------------------------------------
  #
  #     tabPanel(
  #       "Plots",
  #       h3("Index Scores"),
  #       plotOutput("ndx.plt"),
  #       h3("Percentiles"),
  #       plotOutput("per.plt")
  #     )
  #   )
  # )
)



