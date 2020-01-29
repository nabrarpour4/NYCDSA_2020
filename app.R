

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US Oil & Gas Trade"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = 'trade', 
                     label = 'U.S. Trade',
                     choices = unique(df_all$type)),
      selectizeInput(inputId = 'prod', 
                     label = 'Production',
                     choices = unique(ng_prod$variable)),
      selectizeInput(inputId = 'supply', 
                     label = 'Supply_tcf',
                     choices = "Reserves (Trillion Cubic Ft.)")),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("trade"), 
      plotOutput('prod'),
      plotOutput('supply')
    )
  )
)

#Server.R
server <- function(input, output) {
  
  output$trade <- renderPlot({
    
    df_all %>%
      filter(type == input$trade) %>% 
      ggplot(aes(x=Date, y=value, colour=variable)) + 
      geom_line() + 
      labs(title='Imports') + 
      xlab(label = 'Date') +
      ylab(label='Billion Cubic Feet')
  })
  
  output$prod <- renderPlot({
    
    ng_prod %>% filter(variable == input$prod) %>%
      ggplot(aes(x=Date, y=value)) + 
      geom_line() +  
      xlab(label='Date') + 
      ylab(label='Billion Cubic Feet') + 
      labs(title='U.S. Natural Gas Production') 
    
  })
  
  output$supply <- renderPlot({
    
    plot(x=ngsupply$Date, 
         y=ngsupply$Proved_Reserves, 
         type='line', 
         col='blue', 
         main='U.S. Natural Gas Proven Reserves', 
         xlab = 'Date', 
         ylab = 'Trillion Cubic Feet', 
         frame.plot = T, 
         tck = 1)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)