library(shiny)
library(ggplot2)
library(dplyr)



load("gettysburg.Rda")
word_lengths <- gettysburg$num_letters
curr_sample <- NULL


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Create a sampling distribution from Gettysburg word lengths"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      # numericInput("pop_proportion", label = HTML("Population proportion (&pi;)"), .2, min = 0, max = 1, step = .01),
      
      numericInput("sample_size", "Sample size (n)", 10, min = 1, step = 1),
      
      actionButton("generate_1_sample", "Generate 1 sample"),
      actionButton("generate_10_sample", "Generate 10 samples"),
      actionButton("generate_100_sample", "Generate 100 samples"),
      
      checkboxInput("show_sample", "Show the last sample", value = TRUE),
      checkboxInput("show_stats", "Show sampling distribution statistics")
      
      # actionButton("generate_1000_sample", "Generate 1000 samples"),
      # sliderInput("x_range", label = "Zoom range", min = 0, max = 1, value = c(0, 1), step = .01, sep = ""),
      # actionButton("reset", "Reset")
      
    ),
    
    # Show a plot of regression line and the residuals
    mainPanel(
      plotOutput("display_sampling_distribution"),
      br(), br(), br(),
      tableOutput("last_sample_table"),
      tableOutput("data_table_stats")
    )
    
    
  )
)




# Define server logic required to draw the regression image
server <- function(input, output) {
  
  
  # start parameters and helper functions for the plot
  
  sampling_distribution <- NULL
  num_1_botton_presses <- 0
  num_10_botton_presses <- 0
  num_100_botton_presses <- 0
  num_1000_botton_presses <- 0
  
  # set the randomization seed so everyone can get the same results...
  set.seed(1)
  
  
  
  
  # the function to create the figure
  output$display_sampling_distribution <- renderPlot({
    
    
    #reset_plot()
    
    
    # makes sure the plots refreshes if the sample size is changed
    input$sample_size
    
    
    # call the reactiveExpression function to update the samples if a button has been pressed
    add_samples()
    
    df_sampling_distribution <- data.frame(means = sampling_distribution)
    
    df_last_point <- data.frame(means = mean(curr_sample))
    
    
    # if the input is null, add one point to the distribution way out of the range so
    # that the figure shows up
    if (is.null(sampling_distribution)) {
      df_sampling_distribution <- data.frame(means = -10)
      df_last_point <- data.frame(means = -10)
    }
    else {   # get ride of the negative point if it still exists after there is real data
      df_sampling_distribution <- df_sampling_distribution %>% filter(means > 0)
      df_last_point <- df_last_point %>% filter(means > 0)
    }
    
    ggplot(df_sampling_distribution, aes(x = means)) +
      geom_dotplot(binwidth = 1/20, method='histodot') +
      #geom_histogram(bins=300) + 
      xlim(0, 11) +
      #ylim(0, 100) +    # this labelmean values (", bar(x), ")"))) +
      ylim(0, .25) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18)) +
      xlab(expression(paste("Sample mean values (", bar(x), ")"))) +
      ylab("Count") + 
      geom_vline(xintercept = mean(word_lengths), col = "red", alpha = .3) + 
      geom_dotplot(data = df_last_point, binwidth = 1/20, method='histodot', col = "green", fill = "green")
    
    
    
  })
  
  # end plot
  
  
  
  
  add_samples <- eventReactive((input$generate_100_sample | input$generate_10_sample | input$generate_1_sample), {
    
    num.samples <- 0
    
    if (input$generate_1_sample > num_1_botton_presses){
      num.samples <- 1
      num_1_botton_presses <<- num_1_botton_presses + 1
    }
    
    
    if (input$generate_10_sample > num_10_botton_presses){
      num.samples <- 10
      num_10_botton_presses <<- num_10_botton_presses + 1
    }
    
    
    if (input$generate_100_sample > num_100_botton_presses){
      num.samples <- 100
      num_100_botton_presses <<- num_100_botton_presses + 1
    }
    
    
    # if (input$generate_1000_sample > num_1000_botton_presses){
    #   num.samples <- 1000
    #    num_1000_botton_presses <<- num_1000_botton_presses + 1
    #   }
    
    
    if (num.samples > 0){
      
      new_stats <- NULL
      for (i in 1:num.samples){
        curr_sample <<- sample(word_lengths, input$sample_size)
        new_stats[i] <- mean(curr_sample)
      }
      
    } else {
      new_stats <- NULL
    }
    
    
    
    sampling_distribution <<- c(sampling_distribution, new_stats)
    
    
  })
  
  
  # reset_plot <- eventReactive((input$pop_proportion | input$sample_size), {
  #  sampling_distribution <<- NULL
  # })
  
  #reset_plot <- eventReactive((input$reset), {
  #  sampling_distribution <<- NULL
  #})
  
  #observeEvent((input$reset), {
  #  sampling_distribution <<- NULL
  #})
  
  observeEvent((input$sample_size), {
    sampling_distribution <<- NULL
  })
  
  
  
  
  
  
  
  
  output$data_table_stats <- renderTable({
    
    # refesh the table for any of these events
    input$generate_1_sample
    input$generate_10_sample
    input$generate_100_sample
    input$sample_size
    
    if (input$show_stats) {
      
      # taking the mean of a null vector produces an error,
      # so using this to avoid that error...
      if (length(sampling_distribution) > 0)
        the.mean <- mean(sampling_distribution)
      else
        the.mean <- NA
      
      
      stat_df <- data.frame(num_samples = length(sampling_distribution),
                            mean = the.mean,
                            SE = sd(sampling_distribution)
      )
      
      rownames(stat_df) <- ""
      
      
      stat_df
    }
    
    
  })
  
  
  
  
  # could add this to view the last sample, but won't bother 
  
  output$last_sample_table <- renderTable({
    
    # refesh the table for any of these events
    input$generate_1_sample
    input$generate_10_sample
    input$generate_100_sample
    input$sample_size
    
    if (input$show_sample) 
      
      if (is.null(curr_sample)) {
        sample_df <- data.frame()
      } else {
        sample_df <- data.frame(X = t(curr_sample))
        sample_df <- mutate(sample_df, last_sample_mean = mean(curr_sample))
      }
    
    
  })
  
  
  
  
  
  
  
}  # end for the server function


shinyApp(ui = ui, server = server)