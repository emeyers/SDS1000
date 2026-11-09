library(shiny)
library(mosaic)


ui <- fluidPage(
  
  
  titlePanel("Find the probability less than X on a normal distribution"),
  
  
  sidebarLayout(
  
    
    sidebarPanel(
      
      numericInput(inputId = "X",  label = "X",  val = 1, step = .1),
      numericInput(inputId = "mu",  label = HTML("&mu;"),  val = 0),
      numericInput(inputId = "sigma",  label = HTML("&sigma;"),  val = 1)
      
    ),
    
    
    mainPanel(
      plotOutput("my_plot")
    )
  
    
  )
  
)


server <- function(input, output) {
  
  output$my_plot <- renderPlot({
    
    the_title <- paste('Pr(X <= ', input$X, ')')
    
    xpnorm(input$X, input$mu, input$sigma, main = the_title)
    
  })
  
  
}

shinyApp(ui = ui, server = server)