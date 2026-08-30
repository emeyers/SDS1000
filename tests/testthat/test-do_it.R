# Tests for do_it(), the "doer" S4 class, and the * method that repeats an
# expression


test_that("do_it() creates a doer object that stores the number of repetitions", {

  the_doer <- do_it(10)

  expect_true(isVirtualClass("doer") == FALSE)
  expect_true(methods::is(the_doer, "doer"))
  expect_equal(the_doer@n, 10)

  # the default is to run the expression a single time
  expect_equal(do_it()@n, 1L)

})




test_that("do_it() * expression repeats the expression the right number of times", {

  many_hellos <- do_it(10) * { "hello" }

  expect_length(many_hellos, 10)
  expect_equal(many_hellos, rep("hello", 10))
  expect_true(is.character(many_hellos))

  # a vector, rather than a data frame, should be returned
  expect_null(dim(many_hellos))

})




test_that("do_it(1) returns a single value", {

  expect_length(do_it(1) * { 5 }, 1)
  expect_equal(do_it(1) * { 5 }, 5)

})




test_that("do_it() re-evaluates the expression each time rather than recycling one value", {

  set.seed(100)
  the_flips <- do_it(50) * rflip(10)

  expect_length(the_flips, 50)
  expect_true(is.numeric(the_flips))

  # 50 draws from a Binomial(10, 0.5) should not all be the same number
  expect_gt(length(unique(the_flips)), 1)

  # all the values must be legal numbers of heads out of 10 flips
  expect_true(all(the_flips >= 0 & the_flips <= 10))

})




test_that("do_it() gives reproducible results when the random seed is set", {

  set.seed(50)
  first_run <- do_it(20) * mean(rnorm(10))

  set.seed(50)
  second_run <- do_it(20) * mean(rnorm(10))

  expect_equal(first_run, second_run)

})




test_that("do_it() evaluates the expression in the environment it was written in", {

  local_multiplier <- 7

  expect_equal(do_it(3) * (local_multiplier * 2), rep(14, 3))

  # variables created inside a function should also be visible
  a_function_that_uses_do_it <- function() {
    values_in_the_function <- c(2, 4, 6)
    do_it(4) * mean(values_in_the_function)
  }

  expect_equal(a_function_that_uses_do_it(), rep(4, 4))

})




test_that("do_it() works with multi-line expressions inside braces", {

  results <- do_it(5) * {
    first_value <- 3
    second_value <- 4
    sqrt(first_value^2 + second_value^2)
  }

  expect_equal(results, rep(5, 5))

})




test_that("do_it() can be used to build a null distribution that ptail() summarizes", {

  set.seed(200)

  # simulate the number of heads in 20 flips of a fair coin
  null_distribution <- do_it(500) * rflip(20)

  expect_length(null_distribution, 500)

  p_value <- ptail(20, null_distribution, lower.tail = FALSE)

  expect_true(p_value >= 0 && p_value <= 1)

})
