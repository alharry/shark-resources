library(shiny)
library(tidyverse)
library(DT)

ui <- fluidPage(
   
   titlePanel("Shark Resources"),
   
   fluidPage(
     title = 'Double-click to edit table cells',
     fluidRow(column(12, hr(), dataTableOutput('x1')))
   )
)

server <- function(input, output) {
 
  d1 = read_rds("../shark-resources.rds")

  output$x1 = renderDataTable(d1)
}

shinyApp(ui = ui, server = server)

