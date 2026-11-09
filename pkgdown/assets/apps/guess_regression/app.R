

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Find the regression coefficients"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      actionButton("newSampleButton", "Generate new sample"),
      actionButton("newParameterButton", "Generate new slope/intercept"),

      sliderInput("a.intercept", label = "Intercept value (a)", min = -6, max = 6, value = 2, step = .1, sep = ""),
      sliderInput("b.slope", label = "Slope value (b)", min = -6, max = 6, value = 2, step = .1),

      # numericInput("a.intercept", label = "Intercept value (a)", value = 2, step = .1),
      # numericInput("b.slope", label = "Slope value (b)", value = 2, step = .1),
      # checkboxInput("show.opt.a.b", label = "Show least squares solution", value = FALSE)

      checkboxInput("show.improvment", label = "Show improvment as colored text", value = FALSE)



    ),

    # Show a plot of regression line and the residuals
    mainPanel(
      plotOutput("display.regression.plot")
    )


  )
)




# Define server logic required to draw the regression image
server <- function(input, output) {


  # start parameters and helper functions for the plot

  # set the randomization seed so everyone can get the same results...
  set.seed(1)


  # global variables :(

  num.points <- 100   # generate 100 points in the sample to be put on the scatter plot

  beta0 <- NULL
  beta1 <- NULL
  sigma <- NULL
  x.vals <- NULL
  y.vals <- NULL
  rss <- Inf
  sample.button.count <- 0
  parameter.button.count <- 0


  # A function to generage a new set of parameters
  generate_parameters <- function() {

    beta0 <<- runif(1, min = -5, max = 5)     # generate the y-intercept to be between -5 and 5
    beta1 <<- runif(1, min = -5, max = 5)     # generate the slope to be between -5 and 5
    sigma <<- runif(1, min = 1, max = 10)     # generate standard deviation for noise in y direction
  }


  # # start off by using the generate_parameters() function to initialize the parameters
  generate_parameters()


  # A function to generate a sample of data
  generate_sample <- function(num.points)
  {
    # generate the x values to be between -5 and 5
    x.vals <<- runif(num.points, min = -5, max = 5)
    y.vals <<- beta0 + beta1 * x.vals + rnorm(num.points, 0, 5)
  }


  generate_sample(num.points)



  # the function to create the figure
  output$display.regression.plot <- renderPlot({


    # update plot when new a new sample or new parameters are generated make the residuals red
    if (input$newSampleButton >= sample.button.count) {
      generate_sample(num.points)
      sample.button.count <<- sample.button.count + 1
    }


    if (input$newParameterButton >= parameter.button.count) {
      generate_parameters()
      generate_sample(num.points)
      parameter.button.count <<- parameter.button.count + 1
    }




    # calculate the RSS
    y.hat.values <- input$a.intercept + input$b.slope * x.vals
    # rmse <- round(sqrt(mean((y.hat.values - y.vals)^2)), 3)
    rss.new <- round(sum((y.hat.values - y.vals)^2), 3)


    if (rss.new < rss)
      rss.col <- "green"
    else
      rss.col <- "red"

    rss <<- rss.new


    # if the optimal coefficients have been found set the regression line segments to green
    lin.fit <- lm(y.vals ~ x.vals)
    the.coefs <- coef(lin.fit)

    a.opt <- round(the.coefs[1], 1)
    b.opt <- round(the.coefs[2], 1)


    # set the color of the residuals
    the.col <- "red"
    if ((input$a.intercept == a.opt) && (input$b.slope == b.opt))
      the.col <- "green"

    if (!input$show.improvment)
      rss.col <- "black"

    # plot(x.vals, y.vals, xlab = "x", ylab = "y", xlim = c(-5, 5), ylim = c(-30, 30),
    #     main = bquote(hat(.("y")) == ~~ .(input$a.intercept) ~~ + ~~ .(input$b.slope) %.% .("x") ~~ ~~ ~~ ~~ RSS == .(rss)))


    plot(x.vals, y.vals, xlab = "x", ylab = "y", xlim = c(-5, 5), ylim = c(-30, 30))
    title(bquote(hat(.("y")) == ~~ .(input$a.intercept) ~~ + ~~ .(input$b.slope) %.% .("x") ~~ ~~ ~~ ~~ RSS == .(rss)), col.main = rss.col)



    # an example of how one can make only part of the title text colored (a bit complicated with the forumla)
    #title(expression("title (" * phantom("slope=1)") * ")"), col.main = "black")
    #title(expression(phantom("title (") * "slope=1"), col.main = "red")




    # add line segments for the residuals
    y.point.on.line <- input$b.slope * x.vals + input$a.intercept
    segments(x.vals, y.point.on.line, x1 = x.vals, y1 = y.vals, col = the.col)  #, col = par("fg"), lty = par("lty"), lwd = par("lwd"))


    # can print the optimal a and b for debugging purposes...
    # print(a.opt)
    # print(b.opt)


    # add lines which should make it easier to see the x and y intercepts
    abline(v = 0, col = "gray")
    abline(h = 0, col = "gray")


    if ( (!is.na(input$a.intercept))  && (!is.na(input$b.slope))) {
      abline(a = input$a.intercept, b = input$b.slope, col = "red")
    }



  })











  # end plot



}  # end for the server function


shinyApp(ui = ui, server = server)
















