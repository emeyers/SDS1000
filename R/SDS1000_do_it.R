
#' Repeats a process many times and returns a vector of results
#'
#'
#'@note 
#' This is a simplified version of the mosiac do() that returns a vector only.
#' The idea and code are based on the mosaic do() function, which can be found
#' on github page at: https://github.com/ProjectMOSAIC/mosaic/blob/master/R/do.R
#' The code uses S4 objects.
#'
#'   
#' @param n The number of times to repeat the process.
#' 
#' @param null_dist A vector of data consistent with a null hypothesis.
#'   
#' @param lower.tail A Boolean indicating whether one should return the
#'   proportion of points less than or equal to the obs_stat value.
#'
#' @examples
#'  many_hellos <- do_it(10) * { "hello" }
#'
#' @export
do_it <- function(n=1L) {
  new('doer', n=n)
}





# S4 class definition for runner so that * is appropriately dispatched
#' @export

setClass('doer',
         representation = representation(n='numeric'),
         prototype = prototype(n=1))




# create the * method
# first argument is a runner, the second is any expression
#' @export

setMethod(
  "*",
  signature(e1 = "doer", e2="ANY"),
  function (e1, e2) {

    # capture the expression that was given as the second argument in the
    # multiplication (rather than its value), along with the environment that
    # the expression was written in. S4 dispatch has already forced e2 by this
    # point, so the expression has to be recovered with substitute() rather
    # than with a promise-based capture such as lazyeval::f_capture().
    e2_expression <- substitute(e2)
    e2_environment <- parent.frame()

    # get n from the n argument in the do_it() constructor
    n = e1@n

   # evaluate the expression n times
   sapply(integer(n), function(...) { eval(e2_expression, e2_environment) } )

})

