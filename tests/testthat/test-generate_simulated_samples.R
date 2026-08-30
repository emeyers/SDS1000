# Tests for the functions that generate simulated samples


test_that("rsprinkles() returns a factor sample of the right size and colors", {

  the_sprinkles <- rsprinkles(50)

  expect_length(the_sprinkles, 50)
  expect_true(is.factor(the_sprinkles))

  expected_colors <- c("green", "orange", "pink", "red", "white", "yellow")
  expect_equal(levels(the_sprinkles), expected_colors)

  # every sampled value has to be one of the six sprinkle colors
  expect_true(all(as.character(the_sprinkles) %in% expected_colors))

  # a sample of size 1 should still work
  expect_length(rsprinkles(1), 1)

})




test_that("rsprinkles() samples white sprinkles most often", {

  set.seed(100)
  the_sprinkles <- rsprinkles(5000)

  the_proportions <- prop.table(table(the_sprinkles))

  # white sprinkles are generated with probability 0.30, which is roughly
  # twice as often as any of the other colors
  expect_equal(names(which.max(the_proportions)), "white")
  expect_equal(as.numeric(the_proportions["white"]), 0.30, tolerance = 0.03)

})




test_that("rapprovals() returns an ordered factor with two levels by default", {

  the_approvals <- rapprovals(30)

  expect_length(the_approvals, 30)
  expect_true(is.ordered(the_approvals))
  expect_equal(levels(the_approvals), c("disapprove", "approve"))

})




test_that("rapprovals() returns four ordered levels when degree_of_approval is TRUE", {

  the_approvals <- rapprovals(30, degree_of_approval = TRUE)

  expect_length(the_approvals, 30)
  expect_true(is.ordered(the_approvals))
  expect_equal(levels(the_approvals),
               c("strongly disapprove", "disapprove", "approve", "strongly approve"))

})




test_that("rflip_sequence() returns the requested number of flips", {

  expect_length(rflip_sequence(20), 20)
  expect_length(rflip_sequence(1), 1)

})




test_that("rflip_sequence() uses the outcome names requested by output_type", {

  # using prob = 1 makes the outcome deterministic, so the names can be checked
  expect_equal(rflip_sequence(5, prob = 1, output_type = "name"), rep("H", 5))
  expect_equal(rflip_sequence(5, prob = 0, output_type = "name"), rep("T", 5))

  expect_equal(rflip_sequence(5, prob = 1, output_type = "long name"), rep("Heads", 5))
  expect_equal(rflip_sequence(5, prob = 0, output_type = "long_name"), rep("Tails", 5))

  expect_equal(rflip_sequence(5, prob = 1, output_type = "numeric"), rep(1, 5))
  expect_equal(rflip_sequence(5, prob = 0, output_type = "numeric"), rep(0, 5))

  # "name" is the default
  expect_true(all(rflip_sequence(10) %in% c("H", "T")))

})




test_that("rflip_sequence() gives errors for invalid arguments", {

  expect_error(rflip_sequence(0), "must be at least one")
  expect_error(rflip_sequence(-5), "must be at least one")

  expect_error(rflip_sequence(10, prob = -0.1), "must be between 0 and 1")
  expect_error(rflip_sequence(10, prob = 1.5), "must be between 0 and 1")

  expect_error(rflip_sequence(10, output_type = "words"),
               "must be set to either")

})




test_that("rflip() returns a count of heads between 0 and num_flips", {

  set.seed(100)
  the_count <- rflip(10)

  expect_length(the_count, 1)
  expect_true(the_count >= 0 && the_count <= 10)

  # deterministic cases
  expect_equal(rflip(10, prob = 1), 10)
  expect_equal(rflip(10, prob = 0), 0)

})




test_that("rflip() returns a proportion when report_proportion is TRUE", {

  expect_equal(rflip(10, prob = 1, report_proportion = TRUE), 1)
  expect_equal(rflip(10, prob = 0, report_proportion = TRUE), 0)

  set.seed(100)
  the_proportion <- rflip(20, report_proportion = TRUE)
  expect_true(the_proportion >= 0 && the_proportion <= 1)

})




test_that("rflip() gives errors for invalid arguments", {

  expect_error(rflip(0), "must be at least one")
  expect_error(rflip(10, prob = -1), "must be between 0 and 1")
  expect_error(rflip(10, prob = 2), "must be between 0 and 1")

})




test_that("rroll() returns counts that add up to the number of rolls", {

  set.seed(100)
  the_rolls <- rroll(100)

  expect_length(the_rolls, 6)
  expect_equal(sum(the_rolls), 100)
  expect_equal(names(the_rolls), as.character(1:6))
  expect_true(all(the_rolls >= 0))

})




test_that("rroll() uses the probabilities and outcome names that are given", {

  # a four sided die
  the_rolls <- rroll(50, prob = rep(1/4, 4))
  expect_length(the_rolls, 4)
  expect_equal(sum(the_rolls), 50)
  expect_equal(names(the_rolls), as.character(1:4))

  # custom outcome names
  the_names <- c("one", "two", "three", "four", "five", "six")
  named_rolls <- rroll(50, outcome_names = the_names)
  expect_equal(names(named_rolls), the_names)

  # a die that can only land on one side
  loaded_rolls <- rroll(30, prob = c(0, 0, 0, 0, 0, 1))
  expect_equal(as.vector(loaded_rolls), c(0, 0, 0, 0, 0, 30))

})




test_that("cnorm() returns the lower critical value when side is 'lower'", {

  expect_equal(cnorm(0.95, side = "lower"), qnorm(0.025))
  expect_equal(cnorm(0.90, 10, 5, side = "lower"), qnorm(0.05, 10, 5))

  # "upper" is the first, and therefore the default, choice
  expect_equal(cnorm(0.95), cnorm(0.95, side = "upper"))

  expect_error(cnorm(0.95, side = "middle"))

})




test_that("ct() returns the lower critical value when side is 'lower'", {

  expect_equal(ct(0.95, df = 10, side = "lower"), qt(0.025, df = 10))

  # "upper" is the first, and therefore the default, choice
  expect_equal(ct(0.95, df = 10), ct(0.95, df = 10, side = "upper"))

  expect_error(ct(0.95, df = 10, side = "middle"))

  # a t-distribution has fatter tails than the normal distribution
  expect_gt(ct(0.95, df = 5, side = "upper"), cnorm(0.95, side = "upper"))

})




test_that("rflip_count() still works and matches rflip()", {

  expect_equal(rflip_count(10, prob = 1), 10)
  expect_equal(rflip_count(10, prob = 0), 0)
  expect_equal(rflip_count(10, prob = 1, report_proportion = TRUE), 1)

  set.seed(100)
  from_rflip_count <- rflip_count(20)

  set.seed(100)
  from_rflip <- rflip(20)

  expect_equal(from_rflip_count, from_rflip)

})




test_that("the deprecated get_sprinkle_sample() warns but still returns a sample", {

  local_lifecycle_warnings()

  expect_warning(the_sprinkles <- get_sprinkle_sample(25), "deprecated")

  expect_length(the_sprinkles, 25)
  expect_true(is.factor(the_sprinkles))
  expect_equal(levels(the_sprinkles), levels(rsprinkles(1)))

})




test_that("the deprecated get_approval_sample() warns but still returns a sample", {

  local_lifecycle_warnings()

  expect_warning(the_approvals <- get_approval_sample(25), "deprecated")
  expect_length(the_approvals, 25)
  expect_equal(levels(the_approvals), c("disapprove", "approve"))

  expect_warning(detailed_approvals <- get_approval_sample(25, degree_of_approval = TRUE),
                 "deprecated")
  expect_length(detailed_approvals, 25)
  expect_equal(levels(detailed_approvals),
               c("strongly disapprove", "disapprove", "approve", "strongly approve"))

})
