



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


