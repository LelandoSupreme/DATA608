library(dplyr)
library(plotly)
library(shiny)

full_df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv', col.names = c("Cause","State","Year","Deaths","Population","Crude_Rate"), stringsAsFactors = FALSE)

# Define UI ----
ui <- fluidPage(
    titlePanel("Question 1"),
    
    headerPanel("Crude Mortality Rate for Each State - 2010"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput('cse', 'Cause', unique(full_df$Cause), selected='Neoplasms'),
        actionButton('action', label = "Toggle Sort"),

        hr(),
        fluidRow(column(2, verbatimTextOutput("value")))),
      
      mainPanel(
        plotlyOutput('plot1')
      )
      
    )
  )

# Define server logic ----
server <- function(input, output) {
  
  selectedData <- reactive({
    dfSlice <- full_df %>% 
      filter(Year == 2010, Cause == input$cse) %>% 
      mutate(State = factor(State, levels = unique(State)[order(Crude_Rate, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
      
  })
    
  output$plot1 <- renderPlotly({
    
    dfSlice <- full_df %>% 
      filter(Year == 2010, Cause == input$cse) %>% 
      mutate(State = factor(State, levels = unique(State)[order(Crude_Rate, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
    
    plot_ly(selectedData(), x = ~State, y = ~Crude_Rate, color = ~State, type='bar',
            mode = 'lines')
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)