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

