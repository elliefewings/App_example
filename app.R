#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(reshape)
library(ggplot2)
library(dplyr)

#Load data
load("ExAC_nonTCGA_csqfiltered_shiny_allmiss_short_20180316.Rdata")


# Define UI for application that draws a histogram
ui <- fluidPage( 
   fluidRow(
    DT::dataTableOutput("table"))
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###Set main table based on whether genes are searched
  
  output$table <- DT::renderDataTable({
    DT::datatable(vv, options = list(lengthMenu = c(10, 100, 200), pageLength = 10))
    })

}  

# Run the application 
shinyApp(ui = ui, server = server)

