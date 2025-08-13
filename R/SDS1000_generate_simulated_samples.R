
#' Generate a random sample of sprinkle colors
#'
#' This function generates a fictional sample of data of sprinkles.
#' 
#' @param n The sample size.
#' 
#'
#' @examples
#'  # Generate a sample from 10 sprinkle colors
#'  get_sprinkle_sample(10)
#'
#' @export
get_sprinkle_sample <- function(n) {
  
  the_colors <- as.factor(c("green", "orange",  "pink",   "red",   "white",  "yellow"))
  the_proportions <- c(0.15,   0.16,   0.12,   0.155,   0.30,   0.115)
  
  sample(the_colors, n, replace = TRUE, prob = the_proportions)
  
}





#' Generate a random sample of a president's approval ratings
#'
#' This function generates a sample of fictional data of people approve
#' of the president.
#' 
#' @param n The sample size.
#' 
#' @param degree_of_approval A Boolean that if set to TRUE will return a vector
#'   with the levels "strongly disapprove", "disapprove",  "approve", "strongly
#'   approve". If this is set to false, then a vector that only has the levels
#'   "disapprove",  "approve" will be returned.
#'
#' @examples
#'  # Generate a sample from 10 individuals
#'  get_approval_sample(10)
#'
#' @export
get_approval_sample <- function(n, degree_of_approval = FALSE) {


  if (degree_of_approval == FALSE) {

    approval_categories <- ordered(c("disapprove",  "approve"), levels = c("disapprove",  "approve"))
    the_proportions <- c(0.41,   0.59)

  } else {

    level_names <- c("strongly disapprove", "disapprove",  "approve", "strongly approve")
    approval_categories <- ordered(level_names, levels = level_names)
    the_proportions <- c(0.24,   0.17,   0.23,   0.36)

  }

  sample(approval_categories, n, replace = TRUE, prob = the_proportions)

}





#' Generates a sequence of coin flips
#'
#'   
#' @param num_flips The number of times to flip the coin.
#' 
#' @param prob The probability of generated a "heads" on each flip.
#' 
#' @param output_type A string that must either be set to "numeric", "name" or
#'   "long name". If this string is set to "numeric" the output values are 0's
#'   and 1's, where a 1 is a "heads". If this string is set to "name" the output
#'   values are "H" and "T", where a "H" is a "heads". If this string is set
#'   to "long name" the output values are "Heads" and "Tails".
#'   
#' @examples
#'  set.seed(100)
#'  rflip_sequence(10, output_type = "numeric")
#'
#' @export
rflip_sequence <- function(num_flips = 1, prob = .5, output_type = "name"){
  
  # do some sanity checking of the arguments
  if (num_flips < 1) {
    stop("The number of coin flips (num_flips) must be at least one")
  }
  
  
  if ((prob < 0) || (prob > 1)) {
    stop("The probability of getting heads (prob) must be between 0 and 1")
  }
  
  
  if (output_type == "name") {
    
    outcome_names <- c("T", "H")
    
  } else if ((output_type == "long name") || (output_type == "long_name")) {
    
    outcome_names <- c("Tails", "Heads")
    
  } else if (output_type == "numeric"){
    
    outcome_names <- c(0, 1)
    
  } else {
    
    stop("outcome_names must be set to either 'name', 'long_name', or 'numeric'")
    
  }
  
  
  flip_sequence <- outcome_names[rbinom(num_flips, 1, prob) + 1]
  
  
  flip_sequence
  
}






#' Gives a count/proportion from simulating n coin flips
#'
#'   
#' @param num_flips The number of times to flip the coin.
#' 
#' @param prob The probability of generated a "heads" on each flip.
#' 
#' @param report_proportion A Boolean that if set to TRUE will return the
#'   proportion of coin flips that were "heads" otherwise it returns the number
#'   of coin flips that were "heads".
#'   
#' @examples
#'  set.seed(100)
#'  rflip_count(10)
#'
#'
#' @export
rflip_count <- function(num_flips = 1, prob = .5, report_proportion = FALSE) {
  
  # do some sanity checking of the arguments
  if (num_flips < 1) {
    stop("The number of coin flips (num_flips) must be at least one")
  }
  
  
  if ((prob < 0) || (prob > 1)) {
    stop("The probability of getting heads (prob) must be between 0 and 1")
  }
  
  
  head_amount <- rbinom(1, num_flips, prob)
  
  if (report_proportion == TRUE) {
    
    head_amount <- head_amount/num_flips
  }
  
  
  head_amount
  
}






#' Generates a vector of random die rolls
#'
#'  This function simulates rolling a k-sided die n times and returns the number
#'  of times each of the k outcomes occured, where the probability of the k
#'  outcomes is given by the vector prob. This function is just a wrapper for
#'  the `rmultinom()` function.
#'   
#' @param num_rolls The number of times to roll the die.
#' 
#' @param prob The probability of getting each side of the die on each roll
#' 
#' @param outcome_names A vector of names for the outcomes. Default values is
#'  of NULL returns sequential integers as the names of the vector.  
#'   
#' @examples
#'  set.seed(100)
#'  # roll a 6-sided fair die (the default) 100 times
#'  rroll(100, outcome_names = c("one", "two", "three", "four", "five", "six"))
#'
#' @export
rroll <- function(num_rolls, prob = rep(1/6, 6), outcome_names = NULL) {
  
  the_rolls <- as.vector(rmultinom(1, size = num_rolls, prob = prob))
  
  if (is.null(outcome_names)) {
    names(the_rolls) <- 1:length(prob)
  } else {
    names(the_rolls) <- outcome_names
  }
  
  the_rolls
  
}


