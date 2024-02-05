server <- function(input, output, session) {
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  require(cognitive.index.lookup)
  # To allow shinylive running, functions are directly sourced:
  # source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/index_from_raw.R")
  # source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/plot_index.R")
  
    
  dat <- reactive({
    data.frame(
      "id" = "1",
      "ab" = input$ab,
      "age" = input$age,
      "imm" = input$rs1,
      "vis" = input$rs2,
      "ver" = input$rs3,
      "att" = input$rs4,
      "del" = input$rs5,
      stringsAsFactors = FALSE
    )
    
  })
  
  dat_u <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    req(input$file1)
    
    read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote
    ) 
  })
  
  dat_f <- reactive({
    if (input$type == 1) {
      dat()
    } else if (input$type == 2) {
      dat_u()
    }
  })
  
  index_p <- reactive({
    dat_f() |> index_from_raw(
      indx = index_table,
      version.col = "ab",
      age.col = "age",
      raw_columns = c("imm", "vis", "ver", "att", "del")
    )
  })
  
  
  output$ndx.tbl <- renderTable({
    index_p() |>
      select("id", "ab", contains("_is")) |>
      setNames(c("ID", "ab", "imm", "vis", "ver", "att", "del", "Total"))
  })
  
  output$per.tbl <- renderTable({
    index_p() |>
      select("id", "ab", contains("_per")) |>
      setNames(c("ID", "ab", "imm", "vis", "ver", "att", "del", "Total"))
  })
  
  
  output$ndx.plt <- renderPlot({
    index_p() |> plot_index(sub_plot = "_is", facet.by = "ab")
  })
  
  output$per.plt <- renderPlot({
    index_p() |> plot_index(sub_plot = "_per", facet.by = "ab")
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "index_lookup.csv",
    content = function(file) {
      write.csv(index_p(), file, row.names = FALSE)
    }
  )
}