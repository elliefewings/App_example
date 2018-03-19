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
  titlePanel("Explore ExAC non-TCGA ver0.3.1 data"),
  h4("Protein Affecting Variants: Loss of function, inframe indels, and predicted deleterious and damaging missense variants (by SIFT and PolyPhen respectively)"),
  h4("Loss of function Variants: stop gained, stop lost, start lost, frameshift variant, splice donor and splice acceptor variants"),
  fluidRow(h4("")),
  # Create a new Row in the UI for selectInputs
  fluidRow(column(3, 
                  wellPanel(
                    textInput("gene", "Gene:", ""),
                    actionButton("search", "Search"),
                    checkboxInput("match", "Match gene name exactly", value=FALSE)),
                  wellPanel(
                    selectInput("lof","Variant impact:",c("All", "Protein Affecting", "Loss of Function")),
                    selectInput("af", "Variant Allele Frequency:", c("All", "<0.05", "<0.01"))))),
  fluidRow(
    DT::dataTableOutput("table"))
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #All data for start
  fullInput <- reactive({
    data <- vv
    if (input$lof == "Protein Affecting") {
      data <- data %>% filter(VAR_effect == "PAV" | VAR_effect == "LOF")
    }
    if (input$lof == "Loss of Function") {
      data <- data %>% filter(VAR_effect == "LOF")
    }
    if (input$af == "<0.05") {
      data <- data %>% filter(AF < 0.05)
    }
    if (input$af == "<0.01") {
      data <- data %>% filter(AF < 0.01)
    }
    data
  })
  
  #Filter on gene
  geneInput <- eventReactive(input$search, {
    data <- vv
    if (input$gene != "" & input$match == FALSE) {
      data <- data %>% filter(grepl(toupper(input$gene), SYMBOL))
    }
    if (input$gene != "" & input$match == TRUE) {
      data <- data %>% filter(SYMBOL == toupper(input$gene))
    }
  data
  })
  
  # Filter data based on selections
  datasetInput <- reactive({
    data <- geneInput()
    if (input$lof == "Protein Affecting") {
      data <- data %>% filter(VAR_effect == "PAV" | VAR_effect == "LOF")
    }
    if (input$lof == "Loss of Function") {
      data <- data %>% filter(VAR_effect == "LOF")
    }
    if (input$af == "<0.05") {
      data <- data %>% filter(AF < 0.05)
    }
    if (input$af == "<0.01") {
      data <- data %>% filter(AF < 0.01)
    }
    data})
  
  ###Set main table based on whether genes are searched
  
  output$table <- DT::renderDataTable({
    DT::datatable(fullInput(), options = list(lengthMenu = c(10, 100, 200), pageLength = 10))
    })
  
  observeEvent(input$search, {
    output$table <- DT::renderDataTable({
      DT::datatable(datasetInput(), options = list(lengthMenu = c(10, 100, 200), pageLength = 10))
    })
  })
}  

# Run the application 
shinyApp(ui = ui, server = server)

