# Tests for the functions that plot distributions
#
# These functions are called for the plot they draw rather than for a value
# they return, so the tests check that the plots can be drawn without error and
# that the default range of x-values makes sense for the distribution. All the
# plotting is done on a throw-away pdf device so that nothing appears on screen.


# the x-axis range that was actually used by the most recent plot
plotted_x_range <- function() {
  graphics::par("usr")[1:2]
}




test_that("plot_norm() draws a plot centered on the mean", {

  local_null_graphics_device()

  expect_no_error(plot_norm())

  # the default range is the mean plus or minus 5 standard deviations
  plot_norm(100, 15)
  x_range <- plotted_x_range()

  expect_lt(x_range[1], 100 - 15 * 4)
  expect_gt(x_range[2], 100 + 15 * 4)

  # the plot should be roughly symmetric around the mean
  expect_equal(mean(x_range), 100, tolerance = 1)

})




test_that("plot_norm() respects the from, to and add arguments", {

  local_null_graphics_device()

  plot_norm(0, 1, from = -2, to = 2)
  x_range <- plotted_x_range()

  expect_lt(x_range[1], -2 + 0.5)
  expect_gt(x_range[2], 2 - 0.5)

  # adding a curve to an existing histogram should not fail
  set.seed(200)
  hist(rnorm(200, 100, 15), prob = TRUE)
  expect_no_error(plot_norm(100, 15, add = TRUE, col = "blue", lwd = 2))

})




test_that("plot_t() draws a plot centered on zero", {

  local_null_graphics_device()

  plot_t(20)
  x_range <- plotted_x_range()

  expect_lt(x_range[1], 0)
  expect_gt(x_range[2], 0)
  expect_equal(mean(x_range), 0, tolerance = 0.1)

  # a t-distribution with few degrees of freedom is more spread out
  plot_t(5)
  wide_range <- plotted_x_range()

  plot_t(50)
  narrow_range <- plotted_x_range()

  expect_gt(diff(wide_range), diff(narrow_range))

})




test_that("plot_t() can be added to an existing plot", {

  local_null_graphics_device()

  set.seed(200)
  hist(rt(200, 20), prob = TRUE)

  expect_no_error(plot_t(20, add = TRUE, col = "blue", lwd = 2))

})




test_that("plot_chisq() does not draw negative x-values", {

  local_null_graphics_device()

  plot_chisq(5)
  x_range <- plotted_x_range()

  # a chi-squared distribution only takes on non-negative values, so the plot
  # should start at (or just below, because of the axis padding) zero
  expect_lt(x_range[1], 1)
  expect_gt(x_range[1], -2)

  # the range should extend well past the degrees of freedom
  expect_gt(x_range[2], 5 + 5 * sqrt(2 * 5) - 1)

})




test_that("plot_chisq() shifts to the right as the degrees of freedom increase", {

  local_null_graphics_device()

  plot_chisq(2)
  small_df_range <- plotted_x_range()

  plot_chisq(50)
  large_df_range <- plotted_x_range()

  expect_gt(large_df_range[2], small_df_range[2])

  expect_no_error(plot_chisq(5, add = FALSE, col = "blue", lwd = 2))

})




test_that("plot_f() does not draw negative x-values", {

  local_null_graphics_device()

  plot_f(5, 10)
  x_range <- plotted_x_range()

  # an F-distribution only takes on non-negative values
  expect_lt(x_range[1], 1)
  expect_gt(x_range[1], -1)
  expect_gt(x_range[2], 1)

})




test_that("plot_f() can be added to an existing plot", {

  local_null_graphics_device()

  set.seed(200)
  hist(rf(200, 5, 10), prob = TRUE)

  expect_no_error(plot_f(5, 10, add = TRUE, col = "blue", lwd = 2))

})




test_that("the plotting functions accept axis labels and other curve() arguments", {

  local_null_graphics_device()

  expect_no_error(plot_norm(xlab = "z-score", ylab = "density", col = "red", lwd = 3))
  expect_no_error(plot_t(10, xlab = "t", n = 100))
  expect_no_error(plot_chisq(5, xlab = "chi-squared", n = 100))
  expect_no_error(plot_f(5, 10, xlab = "F", n = 100))

})
