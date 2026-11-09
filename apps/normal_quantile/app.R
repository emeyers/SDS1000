library(shiny)
library(mosaic)




ui <- fluidPage(
  
  
  titlePanel("Find the quantiles of a normal distribution"),
  
  
  sidebarLayout(
  
    
    sidebarPanel(
      
      numericInput(inputId = "mu",  label = HTML("&mu;"),  val = 0),
      numericInput(inputId = "sigma",  label = HTML("&sigma;"),  val = 1), 
      sliderInput("xrange", "Quantile range", min = .001, max = 1 - .001, value = c(.30, .70), step = .001)    # hard coded .001 to make it easier to create the UI for now

    ),
    
    
    mainPanel(
      plotOutput("my_plot")
    )
  
    
  )
  
)


server <- function(input, output) {
  
  output$my_plot <- renderPlot({
    
    the_quantiles <- xqnorm(input$xrange, input$mu, input$sigma)
     
    the_title <- paste0('Quantile values:   [', round(the_quantiles[1], 3), "   ", round(the_quantiles[2], 3), "]")
    
    print(input$xrange)
    
    g <- xqnorm(input$xrange, input$mu, input$sigma, return = "plot", verbose = FALSE)
    
    
    g + 
      ggtitle(the_title) + 
      theme_bw() + 
      theme(plot.title = element_text(size=22))

    
    
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)