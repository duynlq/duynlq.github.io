library(shiny)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  sidebarLayout(
    sidebarPanel(
      selectInput("myData", "Choose Dataset",
                  c("iris", "mtcars", "ToothGrowth", "PlantGrowth", "USArrests")),
      uiOutput("datasets")
      
    ),
    mainPanel(
      tabsetPanel(id = 'tab',
                  tabPanel("Datasets",
                           fluidRow(
                             plotOutput("myPlot"),
                             #hr(),
                             DT::dataTableOutput("myTable")
                           )
                  )
                  
      )
    )
  )
)

server <- function(input, output) {
  
  # Extracting column names from datasets
  columns <- reactive({
    switch(input$myData,
           "iris" = names(iris), 
           "mtcars" = names(mtcars),
           "ToothGrowth" = names(ToothGrowth),
           "PlantGrowth" = names(PlantGrowth),
           "USArrests" = names(USArrests),
    )
    
  })
  
  output[["datasets"]] <- renderUI({
    conditionalPanel(condition = "input.tab == 'Datasets'",
                     hr(),
                     HTML(paste0("<b>","Scatterplot","</b>")),
                     selectInput("explanatory", "Select Explanatory Variable (X)",choices = columns()),
                     selectInput("response", "Select Response Variable (Y)",choices = columns()),
                     checkboxInput("showRegLine", "Show Regression Line", TRUE),
                     hr(),
                     HTML(paste0("<b>","Histogram","</b>")),
                     sliderInput("bins", "Adjust Bins", min = 1, max = 50, value = 30),
                     checkboxInput("showDensity", "Show Density Curve", TRUE),
                     hr(),
                     HTML(paste0("<b>","Table","</b>")),
                     checkboxGroupInput("show_vars", "Show Columns",
                                        columns(), selected = columns())
    )
    
  })
  
  output$myPlot <- renderPlot({
    
    attach(get(input$myData))
    
    # Scatterplot
    myFormula = y ~ x
    p1 = ggplot(get(input$myData), aes_string(x = get(input[["explanatory"]]), y = get(input[["response"]]))) +
      geom_point() +
      labs(x = input[["explanatory"]], y = input[["response"]])
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
    
    # Histogram
    p3 = ggplot(get(input$myData), aes_string(x = get(input[["explanatory"]]))) +
      labs(x = input[["explanatory"]], y = "Density") +
      theme_bw(base_size = 16) +
      geom_histogram(aes(x = get(input[["explanatory"]]), y = ..density..), 
                     color = "black", fill = "white", bins = input$bins)
    p4 = {if( input$showDensity)
      p3 + geom_density(alpha = .2, color = "red", fill = "pink")
      else p3}
    
    # Boxplot
    p5 = ggplot(get(input$myData)) + aes_string(x = input[["explanatory"]]) +
      theme_bw(base_size = 16) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1, fill = "yellow")
    
    
    grid.arrange(arrangeGrob(p2),
                 arrangeGrob(p4, p5, ncol=1),
                 ncol=2, widths=c(1,1))
    
  })
  
  output$myTable <- DT::renderDataTable({
    DT::datatable(get(input$myData)[, input$show_vars, drop = FALSE], 
                  options = list(lengthMenu = c(10, 20, 30, 50), pageLength = 10))
  })
}

shinyApp(ui, server)