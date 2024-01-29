require(shiny)
# require(ggplot2)

ui <- fluidPage(
  
  ## -----------------------------------------------------------------------------
  ## Application title
  ## -----------------------------------------------------------------------------
  titlePanel("Calculating cognitive index scores in multidimensional testing.",
             windowTitle = "Cognitive test index calculator"
  ),
  h5("Please note this calculator is only meant as a proof of concept for educational purposes,
     and the author will take no responsibility for the results of the calculator.
     Uploaded data is not kept, but please, do not upload any sensitive data."),
  
  ## -----------------------------------------------------------------------------
  ## Side panel
  ## -----------------------------------------------------------------------------
  
  sidebarPanel(
    h4("Test resultsData format"),
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
        value = 35
      ),
      numericInput(
        inputId = "rs2",
        label = "Visuospatial functions",
        value = 35
      ),
      numericInput(
        inputId = "rs3",
        label = "Verbal functions",
        value = 30
      ),
      numericInput(
        inputId = "rs4",
        label = "Attention",
        value = 35
      ),
      numericInput(
        inputId = "rs5",
        label = "Delayed memory",
        value = 40
      )
    ),
    
    ## -----------------------------------------------------------------------------
    ## File upload
    ## -----------------------------------------------------------------------------
    
    conditionalPanel(
      condition = "input.type==2",
      
      # Input: Select a file ----
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
      ),
      h6("Columns: id, ab, age, imm, vis, ver, att, del."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(
                     Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"
                   ),
                   selected = ","
      ),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = '"'
      ),
    ),
    
    
    ## -----------------------------------------------------------------------------
    ## Download output
    ## -----------------------------------------------------------------------------
    
    # Horizontal line ----
    tags$hr(),
    h4("Download results"),
    
    # Button
    downloadButton("downloadData", "Download")
  ),
  mainPanel(
    tabsetPanel(
      
      ## -----------------------------------------------------------------------------
      ## Summary tab
      ## -----------------------------------------------------------------------------
      tabPanel(
        "Summary",
        h3("Index Scores"),
        htmlOutput("ndx.tbl", container = span),
        h3("Percentiles"),
        htmlOutput("per.tbl", container = span)
      ),
      
      ## -----------------------------------------------------------------------------
      ## Plots tab
      ## -----------------------------------------------------------------------------
      
      tabPanel(
        "Plots",
        h3("Index Scores"),
        plotOutput("ndx.plt"),
        h3("Percentiles"),
        plotOutput("per.plt")
      )
    )
  )
)