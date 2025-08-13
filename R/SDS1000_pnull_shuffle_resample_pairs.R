

#' Shuffles the order of the values in a vector
#'   
#' @param v A vector of data.
#' 
#' @examples
#'  v <- 1:9
#'  shuffle(v)
#'
#' @export
shuffle <- function(v){
  sample(v)
}



#' Calculates the proportion of points in a vector greater than a specified value
#'
#'   
#' @param obs_stat An "observed statistic" value which will be used to assess
#'   how many points in the null_dist are greater than this value.
#' 
#' @param null_dist A vector of data consistent with a null hypothesis.
#'   
#' @param lower.tail A Boolean indicating whether one should return the
#'   proportion of points less than or equal to the obs_stat value.
#'
#' @examples
#'  set.seed(100)
#'  obs_stat <- 3
#'  null_dist <- rnorm(1000)
#'  pnull(obs_stat, null_dist)
#'
#' @export
pnull <- function(obs_stat, null_dist, lower.tail = TRUE) {
  
  
  if (lower.tail == TRUE){
    
    p_val <- sum(null_dist <= obs_stat)/length(null_dist)
    
  } else {
    
    # doing >=  rather than > here (unlike R's pnorm, etc. functions) since this will be used
    #  for computing p-values and I want Pr(X >= obs_stat) the right tail as well
    p_val <-sum(null_dist >= obs_stat)/length(null_dist)
    
  }
  
  p_val
  
}



#' Samples with replacement the same rows from two vectors
#'
#'   
#' @param vector1 A vector of values. 
#' 
#' @param vector2  A vector of values of the same length as vector1.
#'   
#' @examples
#'  vector1 <- 1:26
#'  vector2 <- letters
#'  resample_pairs(vector1, vector2)
#'
#' @return Returns a data frame that has three variables: 
#'  1. The original sample index number
#'  2. The values that were randomly selected from vector1
#'  3. The values that were randomly selected from vector2
#'
#' @export
resample_pairs <- function(vector1, vector2) {
  
  if (length(vector1) != length(vector2)) {
    stop('The two input vectors must be the same length')
  }
  
  n <- length(vector1)
  
  inds_to_use <- sample(1:n, replace = TRUE)
  
  output <- data.frame(inds_to_use, vector1[inds_to_use], vector2[inds_to_use])
  names(output) <- c("original_sample_num", "vector1", "vector2")
  
  output
  
}

