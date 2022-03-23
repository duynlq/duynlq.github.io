library(shiny)
library(tidyverse)
library(ggplot2)
library(ggpmisc)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabs == 'Plot'",
                       selectInput("myData", "Choose From My Datasets: (under dev)",
                                   choices = c("rock", "pressure", "cars")),
                       fileInput("file", "Or Upload Your Own CSV!", multiple = TRUE,
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                       selectInput("explanatory", "Select Explanatory", choices = NULL),
                       
                       radioButtons("plotType", "Select Plot Type",
                                    choices = c("Scatterplot", "Histogram", "Boxplot")),
                       conditionalPanel(condition = "input.plotType == 'Histogram'",
                                        sliderInput("bins", "Adjust Bins", min = 1, max = 50, value = 30),
                                        checkboxInput("showDensity", "Show Density Plot", TRUE)),
                       conditionalPanel(condition = "input.plotType == 'Scatterplot'",
                                        selectInput("response", "Select Response", choices = NULL),
                                        checkboxInput("showRegLine", "Show Regression Line", TRUE))
      ),
      conditionalPanel(condition = "input.tabs =='Table'",
                       checkboxInput("removeNA", "Remove NAs", TRUE)
      )
    ),
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("Plot",
                           plotOutput("plot")),
                  tabPanel("Table",
                           tableOutput("table")),
      )
    )
  )
)

iris = data(iris)
pressure = data(pressure)
mtcars = data(mtcars)

server <- function(input, output,session) {
  
  
  data <- reactive({
    # switch(input$myData,
    #        "rock" = iris,
    #        "pressure" = pressure,
    #        "cars" = mtcars)
    
    req(input$file)
    #req(input$myData)
    
    # get data from file
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
    
    dataset <- vroom::vroom(input$file$datapath, delim = ",")
    
    # let the user know if the data contains no numeric column
    validate(need(ncol(dplyr::select_if(dataset, is.numeric)) != 0,
                  "This dataset has no numeric columns!"))
    dataset
  })
  
  output$table <- renderTable({
    data()
  })
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent( input$file, {
    req(data())
    num_cols <- dplyr::select_if(data(), is.numeric)
    updateSelectInput(session, "explanatory", choices = colnames(num_cols))
  })
  observeEvent( input$file, {
    req(data())
    num_cols <- dplyr::select_if(data(), is.numeric)
    updateSelectInput(session, "response", choices = colnames(num_cols))
  })
  # observeEvent( input$myData, {
  #   req(data())
  #   num_cols <- dplyr::select_if(data(), is.numeric)
  #   updateSelectInput(session, "explanatory", choices = colnames(num_cols))
  # })
  # observeEvent( input$myData, {
  #   req(data())
  #   num_cols <- dplyr::select_if(data(), is.numeric)
  #   updateSelectInput(session, "response", choices = colnames(num_cols))
  # })
  
  # plot histogram
  output$plot <- renderPlot({
    req(!is.null(input$explanatory))
    
    if( input$plotType == "Histogram"){
      #########################################################    
      p1 = ggplot(data(), aes_string(x = input$explanatory)) +
        theme_bw(base_size = 16) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = input$bins)
      p2 = {if( input$showDensity)
        p1 + geom_density(alpha = .2, fill = "#FF6666")
        else p1}
      print(p2)
      #########################################################    
    } else if( input$plotType == "Boxplot") {
      ggplot(data()) + aes_string(x = input$explanatory) +
        theme_bw(base_size = 16) +
        geom_boxplot(outlier.color = "red", outlier.shape = 1, fill = "yellow")
    } else if( input$plotType == "Scatterplot") {
      myFormula = y ~ x
      p1 = ggplot(data(), aes_string(x = input$explanatory, y = input$response)) + 
        geom_point() +
        theme_bw(base_size = 16)
      p2 = {if( input$showRegLine)
        p1 + geom_smooth(method = "lm", color = "red", formula = myFormula) +
          stat_fit_tb(method = "lm",
                      method.args = list(formula = myFormula),
                      tb.vars = c(Parameter = "term", 
                                  Estimate = "estimate", 
                                  "StdErr" = "std.error", 
                                  "italic(t)" = "statistic", 
                                  "italic(P)" = "p.value"),
                      label.y = "bottom", label.x = "right",
                      parse = TRUE) +
          stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                       formula = myFormula, 
                       parse = TRUE)
        else p1}
      print(p2)
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