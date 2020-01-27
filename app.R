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
                       label = 'Natural Gas',
                       choices = unique(colnames(ngexports)))
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

#Server.R
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     ggplot(data=df, aes(x=Date, y=df$value, colour=variable)) + 
       geom_line() + 
       labs(title='U.S. Natural Gas Exports') + 
       xlab(label='Billion Cubic Feet') + 
       ylab(label = 'Date')
   })
   
     
}

# Run the application 
shinyApp(ui = ui, server = server)

