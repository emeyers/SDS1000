# Tests for shuffle(), ptail(), pnull() and resample_pairs()


test_that("shuffle() returns a permutation of the values it is given", {

  the_values <- 1:9
  shuffled_values <- shuffle(the_values)

  expect_length(shuffled_values, length(the_values))
  expect_equal(sort(shuffled_values), the_values)

  # shuffling works on character data too
  shuffled_letters <- shuffle(letters)
  expect_equal(sort(shuffled_letters), letters)

  # shuffle() is a wrapper for sample(), so it inherits sample()'s behavior of
  # treating a single number n as a request to shuffle 1:n
  expect_length(shuffle(7), 7)
  expect_equal(sort(shuffle(7)), 1:7)

})




test_that("shuffle() actually reorders the values", {

  set.seed(100)

  # with 100 values it would be extraordinarily unlikely for a shuffle to
  # return the values in their original order
  expect_false(identical(shuffle(1:100), 1:100))

})




test_that("shuffle() is the same as sample() for a given seed", {

  set.seed(50)
  from_shuffle <- shuffle(1:20)

  set.seed(50)
  from_sample <- sample(1:20)

  expect_equal(from_shuffle, from_sample)

})




test_that("ptail() calculates the proportion of values in the lower tail", {

  the_values <- 1:10

  # lower.tail = TRUE is the default
  expect_equal(ptail(3, the_values), 0.3)
  expect_equal(ptail(3, the_values, lower.tail = TRUE), 0.3)

  expect_equal(ptail(10, the_values), 1)
  expect_equal(ptail(0, the_values), 0)

})




test_that("ptail() calculates the proportion of values in the upper tail", {

  the_values <- 1:10

  expect_equal(ptail(3, the_values, lower.tail = FALSE), 0.8)

  expect_equal(ptail(1, the_values, lower.tail = FALSE), 1)
  expect_equal(ptail(11, the_values, lower.tail = FALSE), 0)

})




test_that("ptail() counts values that are equal to the observed value in both tails", {

  the_values <- c(1, 2, 2, 2, 3)

  # the three 2's are included in both the lower and the upper tail
  expect_equal(ptail(2, the_values, lower.tail = TRUE), 4/5)
  expect_equal(ptail(2, the_values, lower.tail = FALSE), 4/5)

})




test_that("ptail() agrees with a theoretical distribution for a large sample", {

  set.seed(100)
  null_distribution <- rnorm(100000)

  expect_equal(ptail(1.96, null_distribution, lower.tail = FALSE),
               pnorm(1.96, lower.tail = FALSE),
               tolerance = 0.01)

})




test_that("pnull() gives the same answers as ptail()", {

  the_values <- c(1, 5, 5, 8, 12)

  expect_equal(pnull(5, the_values), ptail(5, the_values))
  expect_equal(pnull(5, the_values, lower.tail = FALSE),
               ptail(5, the_values, lower.tail = FALSE))

  expect_equal(pnull(5, the_values), 3/5)
  expect_equal(pnull(5, the_values, lower.tail = FALSE), 4/5)

})




test_that("resample_pairs() returns a data frame with the expected structure", {

  vector1 <- 1:26
  vector2 <- letters

  resampled_data <- resample_pairs(vector1, vector2)

  expect_true(is.data.frame(resampled_data))
  expect_equal(nrow(resampled_data), 26)
  expect_equal(names(resampled_data), c("original_sample_num", "vector1", "vector2"))

})




test_that("resample_pairs() keeps the values in each row paired together", {

  set.seed(100)

  vector1 <- 1:26
  vector2 <- letters

  resampled_data <- resample_pairs(vector1, vector2)

  # each row must come from the same position in the two original vectors
  expect_equal(resampled_data$vector1, vector1[resampled_data$original_sample_num])
  expect_equal(resampled_data$vector2, vector2[resampled_data$original_sample_num])

  # the row indices must be legal positions in the original vectors
  expect_true(all(resampled_data$original_sample_num %in% 1:26))

})




test_that("resample_pairs() samples with replacement", {

  set.seed(100)

  resampled_data <- resample_pairs(1:26, letters)

  # sampling 26 values with replacement essentially always repeats a value
  expect_lt(length(unique(resampled_data$original_sample_num)), 26)

})




test_that("resample_pairs() gives an error when the vectors are different lengths", {

  expect_error(resample_pairs(1:10, letters), "same length")

})
