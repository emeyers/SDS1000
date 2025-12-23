

#' Calculates proportion of values in a particular category
#'
#'   
#' @param v A vector of categorical data.
#' 
#' @param category_name A string specifying the name of a category. The
#'   proportion of values in that category will be returned.
#'
#' @examples
#'  set.seed(100)
#'  sprinkle_sample <- get_sprinkle_sample(100)
#'  get_proportion(sprinkle_sample, "red")
#'
#' @export
get_proportion <- function(v, category_name) {
  
  prop.table(table(v))[category_name]
  
}






#' Calculates the MAD statistic
#'
#' The MAD statistic is the Mean Absolute Difference between the means of
#' several groups. 
#'   
#' @param data A vector of quantitative data.
#' 
#' @param grouping A vector of categorical data indicating which group each
#'   value of the quantitative data belongs to.
#'
#' @examples
#'  set.seed(100)
#'  the_data <- c(rnorm(10, -3), rnorm(10, 0), rnorm(10, 3))
#'  grouping <- c(rep("A", 10), rep("B", 10), rep("C", 10))
#'  get_MAD_stat(the_data, grouping)
#'
#' @export
get_MAD_stat <- function(data, grouping) {


  group_means <- as.vector(by(data, grouping, mean))

  total <- 0
  num_differences <- 0

  for (iGroup1 in 1:(length(group_means) - 1)) {
    for (iGroup2 in (iGroup1 + 1):(length(group_means))) {

      total <- total + abs(group_means[iGroup1] - group_means[iGroup2])
      num_differences <- num_differences + 1

    }
  }

  total/num_differences

} 





#' Calculates the F statistic
#'
#'   
#' @param data A vector of quantitative data.
#' 
#' @param grouping A vector of categorical data indicating which group each
#'   value of the quantitative data belongs to.
#'
#' @examples
#'  set.seed(100)
#'  the_data <- c(rnorm(10, -3), rnorm(10, 0), rnorm(10, 3))
#'  grouping <- c(rep("A", 10), rep("B", 10), rep("C", 10))
#'  get_F_stat(the_data, grouping)
#'
#' @export
get_F_stat <- function(data, grouping, keep_grouping_numeric = FALSE) {

  if (is.numeric(grouping)) {
    
    if (!keep_grouping_numeric) {
      message("converting numeric grouping to factor for F-statistic calculation.")
      grouping <- as.factor(grouping)
    } else {
      message("grouping is numeric. This may lead to incorrect F-statistic calculation.")
    }
    
  }
  
  fit <- aov(data ~ grouping)
  fit_summary <- summary.aov(fit)
  fit_summary[[1]]$`F value`[1]

}






#' Calculates the chi-square statistic
#'
#'   
#' @param observed_counts A vector of observed count data.
#' 
#' @param expected_proportions A vector indicating the expected probability that
#'   data comes from a given group.
#'
#' @examples
#'  set.seed(100)
#'  observed_counts <- c(138, 99, 106, 115, 104, 164)
#'  expected_proportions = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
#'  get_chisqr_stat(observed_counts, expected_proportions)
#'
#' @export
get_chisqr_stat <- function(observed_counts, expected_proportions) {
  
  test_output <- chisq.test(observed_counts, p = expected_proportions)
  test_output$statistic

}





#' Statistics by group
#' 
#' This function computes a summary statistic by group for a given vector of data.
#' 
#' @param data_vector A numeric vector of data.
#'  
#' @param group_vector A factor vector indicating the group membership for each
#'  observation in data_vector.
#'  
#'  @param stat A statistic function to compute separately for each group.
#'
#' @examples
#' # Generate some fictional data
#' set.seed(100)
#' data_vector <- rnorm(100)
#' group_vector <- as.factor(sample(c("A", "B", "C"), size = 100, replace = TRUE))
#' # Compute the mean of data_vector for each group in group_vector
#' stats_by_group(data_vector, group_vector, stat = mean)
#'                                     
#' @export
stats_by_group <- function(data_vector, group_vector, stat = mean) {
  
  if (length(data_vector) != length(group_vector)) {
    stop("data_vector and group_vector must be the same length")
  }
  
  tapply(data_vector, group_vector, stat)
  
}
