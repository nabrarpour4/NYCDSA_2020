#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US Oil & Gas Trade"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = 'Category', 
                     label = 'U.S. Trade',
                     choices = unique(df_all$type)),
      selectizeInput(inputId = 'prod', 
                     label = 'Production',
                     choices = unique(ng_prod$variable))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"), 
      plotOutput('production')
    )
  )
)

#Server.R
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
   df_all %>%
      filter(type == input$Category) %>% 
      ggplot(aes(x=Date, y=value, colour=variable)) + 
      geom_line() + 
      labs(title='Imports') + 
      xlab(label = 'Date') +
      ylab(label='Billion Cubic Feet')
  })

output$production <- renderPlot({
  ng_prod %>% filter(variable == input$prod) %>%
  ggplot(aes(x=Date, y=value)) + 
    geom_line() +  
    xlab(label='Date') + 
    ylab(label='Billion Cubic Feet') + 
    labs(title='U.S. Natural Gas Production') 
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)