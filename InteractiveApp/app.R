library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
  ),
  tagList(
    
    br(), br(),
    column(4,
           wellPanel(
             # Input: Select a file ----
             fileInput("file", "Import Your CSV",
                       multiple = TRUE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             #tags$hr(),
             #tags$hr(),
             
             sliderInput("bins", "Number of Bins", min = 1, max = 50, value = 30),
             selectInput("variable", "Select Variable", choices = NULL),
             selectInput("plot.type", "Select Plot Type",
                         list(Boxplot = "boxplot", Histogram = "histogram", 
                              Density = "density", Bar = "bar")
             ),
             #checkboxInput("show.points", "Show Points", TRUE)
             #wellPanel(
             #radioButtons("type", "Select Plot Type",
             #choices = c("Histogram", "Boxplot")),
             #downloadButton("download", "Save Plot")
           )
    ),
    column(8, plotOutput("distPlot"))
  )
)

server <- function(input, output,session) {
  
  # get data from file
  data <- reactive({
    req(input$file)
    
    # as shown in the book, lets make sure the uploaded file is a csv
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
    
    dataset <- vroom::vroom(input$file$datapath, delim = ",")
    
    # let the user know if the data contains no numeric column
    validate(need(ncol(dplyr::select_if(dataset, is.numeric)) != 0,
                  "This dataset has no numeric columns!"))
    dataset
  })
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent( input$file, {
    req(data())
    num_cols <- dplyr::select_if(data(), is.numeric)
    updateSelectInput(session, "variable", choices = colnames(num_cols))
  })
  
  # plot histogram
  output$distPlot <- renderPlot({
    req(!is.null(input$variable))
    
    x = input$variable
    # bins = seq(1, max(input$variable), length.out = input$bins + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      #xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram")
    
    ggplot(data()) +
      aes_string(x = input$variable) +
      geom_histogram(bins = input$bins) #+
    #geom_col()
    #scale_x_continuous(breaks = seq(min(input$variable), max(input$variable),1), lim = c(min(input$variable), max(input$variable)))
    #geom_bar() +
    #scale_x_binned(n.breaks = input$bins)
  })
  
  # save histogram using downloadHandler and plot output type
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste("histogram", input$extension, sep = ".")
  #   },
  #   content = function(file){
  #     ggsave(file, plot_output(), device = input$type)
  #   }
  # )
}

shinyApp(ui, server)