

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  

  
  # Application title
  titlePanel("US Oil & Gas Trade"),
  
  # Sidebar with a slider input for number of bins 

  

    # Show a plot of the generated distribution
    mainPanel(
      selectizeInput(inputId = 'trade', 
                     label = 'U.S. Trade',
                     choices = unique(df_all$type)),
      
      selectizeInput(inputId = 'prod', 
                     label = 'Production',
                     choices = unique(ng_prod$variable)),
      
      tabsetPanel(
        tabPanel("Trade", plotOutput("trade")),
        tabPanel("Production", plotOutput('prod')),
        tabPanel("Supply", plotOutput('supply')),
        tabPanel("Reserves", plotOutput('reserves'))
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
         y=ngsupply$Proven_Reserves, 
         type='line',
         col='blue', 
         main='U.S. Natural Gas Proven Reserves', 
         xlab = 'Date', 
         ylab = 'Trillion Cubic Feet',
         lwd = 3,
         frame.plot = T, 
         tck = 1)
    
  })
  
  output$reserves <- renderPlot({
    
    a = sort(as.vector(intl_ng[nrow(intl_ng),-1]), decreasing = T)
    barplot(as.numeric(a[,1:5]), 
            names.arg = colnames(a[1:5]), 
            main = 'International Natural Gas Reserves',
            ylab = 'Trillion Cubic Feet',
            ylim = c(0,2000), 
            width = 1, 
            col = c('green', 'blue', 'black', 'red', 'yellow'))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)