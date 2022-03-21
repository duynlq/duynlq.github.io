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
             radioButtons("plotType", "Select Plot Type",
                          choices = c("Histogram", "Boxplot", "Scatterplot", "Density")),
             checkboxInput("showRegLine", "Show Regression (Scatterplot)", TRUE),
             selectInput("variable", "Select Variable", choices = NULL),
             sliderInput("bins", "Number of Bins", min = 1, max = 50, value = 30)
           )
    ),
    column(8, plotOutput("plot"))
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
  output$plot <- renderPlot({
    #req(!is.null(input$variable))
    
    if( input$plotType == "Histogram"){
      ggplot(data()) + aes_string(x = input$variable) +
        geom_histogram(bins = input$bins)
    } else if( input$plotType == "Boxplot") {
      ggplot(data()) + aes_string(x = input$variable) +
        geom_boxplot(outlier.color = "red", outlier.shape = 1)
    } else if( input$plotType == "Scatterplot") {
      p1 = ggplot(data(), aes(IBU, ABV)) + geom_point()
      coefs = lm(ABV~IBU, data = data())$coefficients
      p2 = {if( input$showRegLine)
        p1 + geom_smooth(method = "lm", color = "red")
        else p1}
      print(p2)
    } else if( input$plotType == "Density") {
      ggplot(data()) + aes_string(x = input$variable) +
        geom_density(alpha=0.8)
        #geom_density(aes(fill=factor(cyl)), alpha=0.8)
    }
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