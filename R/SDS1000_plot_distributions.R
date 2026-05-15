

#' Plots a chi-squared distribution
#'   
#' @param df	degrees of freedom (non-negative, but can be non-integer). 
#' 
#' @param from the starting x-value for the plot. If NULL, it will be set to 0 or -5 sd from the mean.
#' 
#' @param to the ending x-value for the plot. If NULL, it will be set to the mean plus 5 sd.
#' 
#' @param n the number of points to use in the plot. Default is 1000.
#' 
#' @param add whether to add the curve to an existing plot (TRUE) or create a new plot (FALSE). Default is FALSE.
#' 
#' @param type the type of plot to create. Default is "l" for a line plot.
#' 
#' @param xlab the label for the x-axis. Default is expression(chi^2).
#' 
#' @param ylab the label for the y-axis. Default is "Probability Density".
#' 
#' @param xlim the limits for the x-axis. If NULL, it will be set to c(from, to).
#' 
#' @param ... additional arguments to be passed to the curve function.
#' 
#' 
#' @examples
#'  plot_chisq(5)
#'
#'  set.seed(100)
#'  hist(rchisq(200, 5), prob = TRUE)
#'  plot_chisq(5, add = TRUE, col = "blue", lwd = 2)
#'
#' @export
plot_chisq <- function(df, 
                       from = NULL, 
                       to = NULL, 
                       n = 1000,
                       add = FALSE, 
                       type = "l",
                       xlab = expression(chi^2),
                       ylab = "Probability Density",
                       xlim = NULL, 
                       ...) {
  
  
  # create default values for the from and to arguments
  spread_length <- 5 * sqrt(2 * df)   # spread x-values to 5 * sd
  if (is.null(from)) {
    from <- max(0, df - spread_length)
  }
  if (is.null(to)) {
    to <- df + spread_length
  }


  
  # version 1
  # x_vals <- seq(from, to, length.out = n)
  # density_vals <- dchisq(x_vals, df)
  # plot(x_vals, density_vals, type = "l",
  #      xlab = xlab,
  #      ylab = ylab,
  #      xlim)
  # abline(h = 0)
  
  
  # version 2
  curve(dchisq(x, df), 
        from = from, 
        to = to,
        n = n,
        add = add,
        xlab = xlab,
        ylab = ylab, 
        xlim = xlim,
        ...)
  
  abline(h = 0)
  
  
  
  # version 3
  # could create a ggplot version similar to mosaic::xpchisq()
  # but can't use abline(v = value, col = "red") etc. so perhaps best to keep
  # with simplier base R version to make it more similar to other code we are using
  
  
}





#' Plots an F-distribution
#'  
#'  
#'  






#' Plots a Normal distribution
#'  





#' Plots a t-distribution
#'  

