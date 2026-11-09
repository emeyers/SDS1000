library(shiny)
library(mosaic)


ui <- fluidPage(
  
  
  titlePanel("Find the probability X falls in an interval on a normal distribution"),
  
  
  sidebarLayout(
  
    
    sidebarPanel(
      
      numericInput(inputId = "mu",  label = HTML("&mu;"),  val = 0),
      numericInput(inputId = "sigma",  label = HTML("&sigma;"),  val = 1), 
      uiOutput("x_range_select")
      
    ),
    
    
    mainPanel(
      plotOutput("my_plot")
    )
  
    
  )
  
)


server <- function(input, output) {
  
  output$my_plot <- renderPlot({
    
    the_title <- paste('Pr(', input$xrange[1], '< X < ', input$xrange[2], ')')
    
    g <- xpnorm(input$xrange, input$mu, input$sigma, return = "plot", verbose = FALSE)
    
    g + 
      ggtitle(the_title) + 
      theme_bw() + 
      theme(plot.title = element_text(size=22))

  })
  
  
  output$x_range_select <- renderUI({
    
    sliderInput("xrange", "x value range", min = (-5 * input$sigma) + input$mu, max = (5 * input$sigma) + input$mu, value = (c(-2, 2) * input$sigma) + input$mu, step = input$sigma/100)
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)