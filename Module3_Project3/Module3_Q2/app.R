library(dplyr)
library(plotly)
library(shiny)
library(sqldf)

full_df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv', col.names = c("Cause","State","Year","Deaths","Population","Crude_Rate"), stringsAsFactors = FALSE)
nat.avg <- sqldf('SELECT Cause, Year, sum(Deaths) as Dths, Sum(Population) as Pop FROM full_df GROUP BY Cause, Year')
nat.avg$Nat_Avg_Crude_Rate <- round((nat.avg$Dths * 100000) / nat.avg$Pop,1)
final_df <- merge(full_df, nat.avg, by = c("Cause", "Year"))

# Define UI ----
ui <- fluidPage(
  titlePanel("Question 2"),
  
  headerPanel("State Crude Rate vs. National Average Crude Rate by Cause"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('cse', 'Cause', unique(full_df$Cause), selected='Neoplasms'),
      selectInput('st', 'State', unique(full_df$State), selected='AL')
    ),
    
    mainPanel(
      plotlyOutput('plot2')
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  selectedData <- reactive({
    dfSlice <- final_df %>% 
      filter(State == input$st, Cause == input$cse)
    
  })
  
  output$plot2 <- renderPlotly({
    
    dfSlice <- final_df %>% 
      filter(State == input$st, Cause == input$cse)
    
    plot_ly(selectedData(), x = ~Year, y = ~Crude_Rate, name = 'State Crude Rate', color = 'orange', type='scatter',
            mode = 'lines') %>% add_trace(y = ~Nat_Avg_Crude_Rate, name = 'Nat Avg Crude Rate', color = 'blue', mode = 'lines')
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)