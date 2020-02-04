

library(shiny)
library(shinydashboard)

ui <- fluidPage(

dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "U.S. Energy Renaissance",
    tags$li(actionLink("GitHub", 
                       label = "", 
                       icon = icon("github"),
                       onclick = "window.open('https://github.com/nabrarpour4/NYCDSA_2020')"),
            class = "dropdown")
  ),
  dashboardSidebar(
    sidebarUserPanel("Nickolas Abrarpour", subtitle = "NYCDSA Fellow"),
    
    sidebarMenu(
      menuItem("Trade", tabName = "trade"),
      menuItem("Production", tabName = "prod"),
      menuItem("Supply", tabName = "supply"),
      menuItem("International Reserves", tabName = "reserves")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "trade",
        mainPanel(
              fluidRow(
                plotOutput('trade'),
          column(4,
                 selectizeInput(inputId = 'trade', 
                                label = 'Imports/Exports',
                                choices = df_all$type,
                                selected =df_all$value)
          ) # end fluidRow for user input
        ) # end tabItem for chart
        ) # end fluidRow for user input
      ), # end tabItem for map
      tabItem(tabName = "prod",
              fluidRow(
                plotOutput("prod")
              ),
                column(4,
                       selectizeInput(inputId = 'prod', 
                                      label = 'Production',
                                      choices = unique(ng_prod$variable))
                ) # end fluidRow for user input
      ), # end tabItem for chart
      tabItem(tabName = "supply",
              fluidRow(
                plotOutput("supply")
      ) # end fluidRow for tabItem 'data'
      ), # end tabItem data
      tabItem(tabName = "reserves",
              fluidRow(
                plotOutput("reserves"))
      )
     )
    )
  )
)
 # end dashboardBody


server <- function(input, output) {
  
  
  output$trade <- renderPlot({
    
    df_all %>%
      filter(type == input$trade) %>% 
      ggplot(aes(x=Date, y=value, colour=variable)) + 
      geom_line() + 
      xlab(label = 'Date') +
      ylab(label='Billion Cubic Feet') +
      labs(title='U.S. Natural Gas Trade') + 
      theme(legend.position = "bottom")
  })
  
  output$prod <- renderPlot({
    
    ng_prod %>% filter(variable == input$prod) %>%
      ggplot(aes(x=Date, y=value/1000)) + 
      geom_line(color="red") +
      xlab(label='Date') + 
      ylab(label='Trillion Cubic Feet') +
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
            col = c('green', 'blue', 'orange', 'black', 'brown'))
    
  })
  
  
}

shinyApp(ui = ui, server = server)

