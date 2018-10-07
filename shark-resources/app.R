library(shiny)
library(tidyverse)
library(DT)

# Read in data
data <- read_rds("../shark-resources.rds") %>% 
  mutate(year = as.integer(year)) %>% 
  select(resource_id, resource, year, DOI, source) %>% 
  rename(ID = resource_id, Resource = resource,
         Year = year, Source = source) %>% 
  arrange(Resource)

# User interface
ui <- function(input, output) {
  navbarPage("Shark Resources",
           tabPanel("References",
                    sidebarLayout(
                      sidebarPanel(
                        h4("Filters"),
                        selectInput('year', 'Year', sort(data$Year, d = T), multiple=TRUE, 
                                    selectize=TRUE, selected = NULL),
                        br(),
                        width = 2
                      ),
                      mainPanel(
                        dataTableOutput('x1'),
                        width = 10
                      )
                    )
           ),
           tabPanel(
             "About",
             includeMarkdown("../readme.md")
           )
  )
}

# Server
server <- function(input, output) {
 
   dataset <- reactive({
     if(is.null(input$year)){data} else {filter(data, Year %in% input$year)}
   })
  
  output$x1 = renderDataTable(dataset(),
    rownames = FALSE, filter = 'top', 
    options = list(searchHighlight = TRUE, selection = list(mode = "single")
    )
  )
}

shinyApp(ui = ui, server = server)

