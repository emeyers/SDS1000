# Test for SDS1000 package

# setup a temporary directory for testing
test_dir <- tempdir()
original_wd <- getwd()

# a helper function to set the root path for testing
setup_test_root_path <- function() {
  # create a temporary directory for the root path
  test_root_path <- file.path(test_dir, "sds1000_test_material")
  if (!dir.exists(test_root_path)) {
    dir.create(test_root_path, recursive = TRUE)
  }

  # set the root path to the temporary directory
  set_class_material_root_path(test_root_path)

  return(test_root_path)
}


test_that("can set and get class material root path", {

  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()

  # check that the root path is set correctly
  expect_equal(get_class_material_root_path(), file.path(test_root_path, "sds1000_material"))

  # cleanup
  unlink(test_root_path, recursive = TRUE)
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})


test_that("can download a github directory", {

  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()
  original_wd <- getwd()

  # download a test directory
  # using a directory that should exist in the test branch
  download_github_directory("homework/homework_-1")

  # check that the files are downloaded
  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_-1")
  expect_true(dir.exists(expected_dir))

  # check for a specific file
  expected_file <- file.path(expected_dir, "homework_-1.Rmd")
  expect_true(file.exists(expected_file))

  # cleanup
  setwd(original_wd)
  unlink(test_root_path, recursive = TRUE)
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})


test_that("goto_directory downloads files when prompted", {

  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()
  original_wd <- getwd()

  # use goto_directory for a test directory, mocking menu and rstudioapi
  testthat::with_mocked_bindings(
    goto_directory("class_code/class_-1", "class -1"),
    menu = function(...) 1,
    `rstudioapi::filesPaneNavigate` = function(...) {}
  )

  # check that the files are downloaded
  expected_dir <- file.path(get_class_material_root_path(), "class_code/class_-1")
  expect_true(dir.exists(expected_dir))

  # check for a specific file
  expected_file <- file.path(expected_dir, "class_-1.Rmd")
  expect_true(file.exists(expected_file))

  # cleanup
  setwd(original_wd)
  unlink(test_root_path, recursive = TRUE)
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})


test_that("can backup and restore a directory", {

  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()

  # create a dummy directory and file to backup
  dir_to_backup <- file.path(get_class_material_root_path(), "homework/homework_test")
  dir.create(dir_to_backup, recursive = TRUE)
  file.create(file.path(dir_to_backup, "test.txt"))

  # backup the directory
  move_to_backup("homework/homework_test")

  # check that the original directory is gone
  expect_false(dir.exists(dir_to_backup))

  # check that the backup exists
  backups <- list_backups()
  expect_true(any(grepl("homework_test", backups)))

  # restore from backup
  restore_from_backup("homework/homework_test")

  # check that the directory is restored
  expect_true(dir.exists(dir_to_backup))
  expect_true(file.exists(file.path(dir_to_backup, "test.txt")))

  # cleanup
  unlink(test_root_path, recursive = TRUE)
  backup_dir <- file.path(dirname(get_class_material_root_path()), "backup")
  if (dir.exists(backup_dir)) {
    unlink(backup_dir, recursive = TRUE)
  }
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})
