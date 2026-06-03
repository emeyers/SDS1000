



#' Plots a Normal distribution
#'  
#' @param mean the mean of the distribution.
#' 
#' @param sd the standard deviation of the distribution.
#' 
#' @param from the starting x-value for the plot. If NULL, it will be set to -5 sd from the mean.
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
#' @examples
#' plot_norm()
#' 
#' set.seed(200)
#' hist(rnorm(200, 100, 15), prob = TRUE)
#' plot_norm(100, 15, add = TRUE, col = "blue", lwd = 2)
#'       
#' @export
plot_norm <- function(mean = 0, 
                      sd = 1,
                      from = NULL,
                      to = NULL,
                      n = 1000,
                      add = FALSE,
                      type = "l",
                      xlab = expression(X),
                      ylab = "Probability Density",
                      xlim = NULL,
                      ...) {
  
  
  if (is.null(from)) {
    from <- mean -5 * sd
  }
  if (is.null(to)) {
    to <- mean + 5 * sd
  }
  
  curve(dnorm(x, mean, sd), 
        from = from, 
        to = to,
        n = n,
        add = add,
        xlab = xlab,
        ylab = ylab, 
        xlim = xlim,
        ...)
  
  abline(h = 0)
  
}





#' Plots a t-distribution
#'  
#' @param df degrees of freedom (non-negative, but can be non-integer).
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
#' @examples
#' plot_t(20)
#' 
#' set.seed(200)
#' hist(rt(200, 20), prob = TRUE)
#' plot_t(20, add = TRUE, col = "blue", lwd = 2)
#'       
#' @export
plot_t <- function(df,
                   from = NULL,
                   to = NULL,
                   n = 1000,
                   add = FALSE,
                   type = "l",
                   xlab = expression(T),
                   ylab = "Probability Density",
                   xlim = NULL,
                   ...) {
  
  
  if (is.null(from)) {
    from <- -5 * sqrt(df / (df - 2))  # mean is 0, so spread is based on sd
  }
  
  if (is.null(to)) {
    to <- 5 * sqrt(df / (df - 2))  # mean is 0, so spread is based on sd
  }
  
  
  curve(dt(x, df), 
        from = from, 
        to = to,
        n = n,
        add = add,
        xlab = xlab,
        ylab = ylab, 
        xlim = xlim,
        ...)
  
  abline(h = 0)
  
}



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
#' @param df1 degrees of freedom for the numerator (non-negative, but can be non-integer).
#' 
#' @param df2 degrees of freedom for the denominator (non-negative, but can be non-integer).
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
#' @examples
#' plot_f(5, 10)
#' 
#' set.seed(200)
#' hist(rf(200, 5, 10), prob = TRUE)
#' plot_f(5, 10, add = TRUE, col = "blue", lwd = 2)
#'       
#' @export
plot_f <- function(df1, 
                   df2,
                   from = NULL,
                   to = NULL,
                   n = 1000,
                   add = FALSE,
                   type = "l",
                   xlab = expression(F),
                   ylab = "Probability Density",
                   xlim = NULL,
                   ...) {
  
  
  # create default values for the from and to arguments
  # note that the F distribution is skewed to the right, so we will use a larger spread for the right tail
  
  mean <- df2 / (df2 - 2)  # mean of F distribution
  sd <- sqrt((2 * df2^2 * (df1 + df2 - 2)) / (df1 * (df2 - 2)^2 * (df2 - 4)))  # sd of F distribution
  spread_length <- 5 * sd   # spread x-values to 5 * sd
  
  if (is.null(from)) {
    from <- max(0, mean - spread_length)
  }
  
  if (is.null(to)) {
    to <- mean + spread_length
  }
  

  curve(df(x, df1, df2), 
        from = from, 
        to = to,
        n = n,
        add = add,
        xlab = xlab,
        ylab = ylab, 
        xlim = xlim,
        ...)
  
  abline(h = 0)
  
}



