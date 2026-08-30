



test_that("cnorm function gets the middle critical values", {

  expect_equal(cnorm(0.95, side = "both"), qnorm(c(0.025, 0.975)))
  
  expect_equal(cnorm(0.90, side = "upper"), qnorm(0.95))
  
  expect_equal(cnorm(0.99, 10, 5, side = "both"), qnorm(c(0.005, 0.995), 10, 5))
  
})





test_that("ct function gets the middle critical values", {
  
  expect_equal(ct(0.95, df = 10, side = "both"), qt(c(0.025, 0.975), df = 10))
  
  expect_equal(ct(0.90, df = 15, side = "upper"), qt(0.95, df = 15))
  
  expect_equal(ct(0.99, df = 5, side = "both"), qt(c(0.005, 0.995), df = 5))
  
})
  



test_that("get_proportion function works", {
  
  the_data <- c("red", "blue", "green", "red", "yellow", "red", "blue", "green", "red")
  
  expected_proportion_red <- sum(the_data == "red") / length(the_data)
  names(expected_proportion_red) <- "red"
  expected_proportion_blue <- sum(the_data == "blue") / length(the_data)
  names(expected_proportion_blue) <- "blue"
  
  expect_equal(get_proportion(the_data, "red"), expected_proportion_red)
  expect_equal(get_proportion(the_data, "blue"), expected_proportion_blue)
  
})






test_that("get_F_stat function works", {
  
  the_data <- mtcars$mpg
  grouping <- as.factor(mtcars$cyl)
  grouping_numeric <- mtcars$cyl

  added_test <- aov(the_data ~ grouping)
  added_summary <- summary.aov(added_test)
  expected_F_stat <- added_summary[[1]]$`F value`[1]
  
  expect_equal(get_F_stat(the_data, grouping), expected_F_stat)
  
  # get_F_stat converts a numeric grouping varaible to a factor and gives a message
  # if a grouping variable is numeric
  expect_message(result_numeric_conversion <- get_F_stat(the_data, grouping_numeric))
  expect_equal(result_numeric_conversion, expected_F_stat)
  
  # get_F_stat with keep_grouping_numeric = TRUE gives a message but does not convert
  expect_message(result_numeric_keep <- get_F_stat(the_data, grouping_numeric, keep_grouping_numeric = TRUE))
  
  added_test2 <- aov(the_data ~ grouping_numeric)
  added_summary2 <- summary.aov(added_test2)
  expected_F_stat2 <- added_summary2[[1]]$`F value`[1]
  expect_equal(result_numeric_keep, expected_F_stat2)
  
})




test_that("get_chisqr_stat function works", {
  
  observed_counts <- c(138, 99, 106, 115, 104, 164)
  expected_proportions = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
  
  added_test <- chisq.test(observed_counts, p = expected_proportions)
  expected_chisqr_stat <- added_test$statistic
  
  expect_equal(get_chisqr_stat(observed_counts, expected_proportions), expected_chisqr_stat)
  
})






test_that("get_MAD_stat function works", {

  # the group means are 1.5, 5.5 and 10.5, so the absolute differences between
  # the pairs of means are 4, 9 and 5, which average to 6
  the_data <- c(1, 2, 5, 6, 10, 11)
  grouping <- c("A", "A", "B", "B", "C", "C")

  expect_equal(get_MAD_stat(the_data, grouping), 6)

})




test_that("get_MAD_stat with two groups is the difference between the two means", {

  the_data <- c(2, 4, 10, 20)
  grouping <- c("A", "A", "B", "B")

  expect_equal(get_MAD_stat(the_data, grouping), abs(3 - 15))

  # the statistic does not depend on the order the groups are in
  expect_equal(get_MAD_stat(rev(the_data), rev(grouping)), abs(3 - 15))

})




test_that("get_MAD_stat is 0 when all the group means are the same", {

  the_data <- c(1, 3, 0, 4, 2, 2)
  grouping <- c("A", "A", "B", "B", "C", "C")

  expect_equal(get_MAD_stat(the_data, grouping), 0)

})




test_that("get_MAD_stat works with a factor grouping variable", {

  the_data <- c(1, 2, 5, 6, 10, 11)
  grouping <- c("A", "A", "B", "B", "C", "C")

  expect_equal(get_MAD_stat(the_data, as.factor(grouping)),
               get_MAD_stat(the_data, grouping))

})




test_that("stats_by_group function works", {

  data_vector <- c(1, 2, 3, 10, 20, 30)
  group_vector <- as.factor(c("A", "A", "A", "B", "B", "B"))

  the_means <- stats_by_group(data_vector, group_vector)

  expect_equal(as.vector(the_means), c(2, 20))
  expect_equal(names(the_means), c("A", "B"))

  # the same answer as calculating the means by hand
  expect_equal(the_means, tapply(data_vector, group_vector, mean))

})




test_that("stats_by_group can use statistics other than the mean", {

  data_vector <- c(1, 2, 30, 10, 20, 300)
  group_vector <- as.factor(c("A", "A", "A", "B", "B", "B"))

  expect_equal(as.vector(stats_by_group(data_vector, group_vector, stat = median)),
               c(2, 20))

  expect_equal(as.vector(stats_by_group(data_vector, group_vector, stat = length)),
               c(3, 3))

  expect_equal(as.vector(stats_by_group(data_vector, group_vector, stat = max)),
               c(30, 300))

})




test_that("stats_by_group works with a character grouping variable", {

  data_vector <- c(1, 2, 3, 10, 20, 30)
  group_vector <- c("A", "A", "A", "B", "B", "B")

  expect_equal(as.vector(stats_by_group(data_vector, group_vector)), c(2, 20))

})




test_that("stats_by_group gives an error when the vectors are different lengths", {

  expect_error(stats_by_group(c(1, 2, 3), as.factor(c("A", "B"))),
               "must be the same length")

})
