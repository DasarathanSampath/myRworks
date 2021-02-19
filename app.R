# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(shiny)
library(ggplot2)

ui <- function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- mpg
        if (input$man != "All") {
            data <- data[data$manufacturer == input$man,]
        }
        if (input$cyl != "All") {
            data <- data[data$cyl == input$cyl,]
        }
        if (input$trans != "All") {
            data <- data[data$trans == input$trans,]
        }
        data
    }))
    
}

server <- function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- mpg
        if (input$man != "All") {
            data <- data[data$manufacturer == input$man,]
        }
        if (input$cyl != "All") {
            data <- data[data$cyl == input$cyl,]
        }
        if (input$trans != "All") {
            data <- data[data$trans == input$trans,]
        }
        data
    }))
    
}