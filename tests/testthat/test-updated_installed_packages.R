# 
# This function doesn't work on the YCRC server
# Can just have students use install.packages() if any additional packages are
#  needed for the class. 
#
#
# test_that("update_installed_packages() works", {
# 
#   xkcd_package_installed <- "xkcd" %in% installed.packages()[, 1]
# 
#   if (xkcd_package_installed ) {
#     remove.packages("xkcd")
#   }
# 
#   update_installed_packages()
# 
#   expect_true("xkcd" %in% installed.packages()[, 1])
# 
#   # if the xkcd package was not installed prior to testing then remove it
#   if (!xkcd_package_installed) {
#     remove.packages("xkcd")
#   }
# 
# })




# This function doesn't seem to work. Not sure I really need it. 

# test_that("can reinstall the package", {
# 
#   expect_no_error({
#     reinstall_package()
#   }, message = "Running reinstall_package() created an error")
#     
#   
# })





# The tests below replace the functions that read the package list from GitHub
# and that install packages with stand-ins, so that the logic in
# update_installed_packages() can be tested without changing the R installation
# of whoever is running the tests.


test_that("update_installed_packages() reads the package list from the class GitHub site", {

  requested_url <- NULL

  testthat::local_mocked_bindings(
    read.csv = function(file, ...) {
      requested_url <<- file
      data.frame(V1 = character(0))
    },
    installed.packages = function(...) matrix("stats", nrow = 1, ncol = 1),
    install.packages = function(...) stop("nothing should have been installed"),
    .package = "utils"
  )

  update_installed_packages()

  expect_equal(requested_url, paste0(get_base_url(), "required_packages.txt"))

})




test_that("update_installed_packages() only installs packages that are missing", {

  installed_packages <- NULL

  testthat::local_mocked_bindings(
    read.csv = function(...) data.frame(V1 = c("stats", "a_missing_package")),
    installed.packages = function(...) matrix(c("stats", "utils"), ncol = 1),
    install.packages = function(pkgs, ...) {
      installed_packages <<- c(installed_packages, pkgs)
      invisible(NULL)
    },
    .package = "utils"
  )

  update_installed_packages()

  expect_equal(installed_packages, "a_missing_package")

})




test_that("update_installed_packages() installs nothing when everything is installed", {

  testthat::local_mocked_bindings(
    read.csv = function(...) data.frame(V1 = c("stats", "utils")),
    installed.packages = function(...) matrix(c("stats", "utils"), ncol = 1),
    install.packages = function(...) stop("nothing should have been installed"),
    .package = "utils"
  )

  expect_silent(update_installed_packages())

})




test_that("update_installed_packages(force_reinstall = TRUE) reinstalls every class package", {

  installed_packages <- NULL

  the_required_packages <- c("stats", "utils", "a_missing_package")

  testthat::local_mocked_bindings(
    read.csv = function(...) data.frame(V1 = the_required_packages),
    installed.packages = function(...) matrix(c("stats", "utils"), ncol = 1),
    install.packages = function(pkgs, ...) {
      installed_packages <<- c(installed_packages, pkgs)
      invisible(NULL)
    },
    .package = "utils"
  )

  update_installed_packages(force_reinstall = TRUE)

  # all the packages are installed in a single call
  expect_equal(installed_packages, the_required_packages)

})




test_that("initial_setup() installs the class packages and sets the root path", {

  the_steps_that_were_run <- character(0)

  testthat::local_mocked_bindings(
    update_installed_packages = function(...) {
      the_steps_that_were_run <<- c(the_steps_that_were_run, "update_installed_packages")
      invisible(NULL)
    },
    set_class_material_root_path = function(...) {
      the_steps_that_were_run <<- c(the_steps_that_were_run, "set_class_material_root_path")
      invisible(NULL)
    }
  )

  # LaTeX takes a long time to install, so it is not installed by default
  expect_output(initial_setup(), "Initial setup complete")

  expect_equal(the_steps_that_were_run,
               c("update_installed_packages", "set_class_material_root_path"))

})
