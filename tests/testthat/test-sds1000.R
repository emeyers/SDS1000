# Test for SDS1000 package

# setup a temporary directory for testing
test_root_path <- tempdir()
original_wd <- getwd()




# a helper function to set the root path for testing
setup_test_root_path <- function() {

  
  # clean up to start with in case any previous tests left files behind
  if (dir.exists(test_root_path)) {
    unlink(test_root_path, recursive = TRUE)
  }
  
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

  # download a test directory
  # using a directory that should exist in the test branch
  download_github_directory("homework/homework_-1")

  # check that the files are downloaded
  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_-1")
  expect_true(dir.exists(expected_dir))

  # check for a specific file
  expected_file <- file.path(expected_dir, "homework_-1.qmd")
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

  # unfortunately can't mock menu so going to just download the file first to avoid call to menu()
  # use goto_directory for a test directory, mocking menu and rstudioapi
  #testthat::with_mocked_bindings(
  #   goto_directory("class_code/class_-1", "class -1"),
  #   menu = function(...) 1,
  #   `rstudioapi::filesPaneNavigate` = function(...) {}
  # )

  download_github_directory("class_code/class_-1")
  goto_class(-1)

  
  # check that the files are downloaded
  expected_dir <- file.path(get_class_material_root_path(), "class_code/class_-1")
  expect_true(dir.exists(expected_dir))

  # check for a specific file
  expected_file <- file.path(expected_dir, "class_-1.qmd")
  expect_true(file.exists(expected_file))

  # check that the working directory is set correctly
  expect_equal(normalizePath(getwd()), normalizePath(expected_dir))
  
  # cleanup
  setwd(original_wd)
  unlink(test_root_path, recursive = TRUE)
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})




test_that("can backup and restore a directory", {

  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()

  # download a test directory to have something to backup
  download_github_directory("homework/homework_-1")
  
  # add a dummy file to the directory to ensure something is there
  dir_to_backup <- file.path(get_class_material_root_path(), "homework/homework_-1")
  file.create(file.path(dir_to_backup, "dummy.txt"))

  # backup the directory
  move_to_backup("homework", -1)
  
  # check that the original directory no longer contains any files
  dir_to_backup <- file.path(get_class_material_root_path(), "homework/homework_-1")
  #expect_false(dir.exists(dir_to_backup))  # might not delete the directory, just its contents
  expect_length(list.files(dir_to_backup), 0) # should be empty
  
  # check that the backup exists
  backups <- list_backups()
  print(backups)
  expect_true(any(grepl("homework_-1", backups)))
  
  # download new copy of homework -1 to test restore
  download_github_directory("homework/homework_-1")
  
  # check that the new copy does not have the dummy file
  expect_false(file.exists(file.path(dir_to_backup, "dummy.txt")))
  
  # restore from backup
  restore_from_backup(list_backups()[1])
  
  # check that the directory is restored
  expect_true(dir.exists(dir_to_backup))
  expect_true(file.exists(file.path(dir_to_backup, "dummy.txt")))
  
  # check that the newly downloaded file is in the backup list
  backups2 <- list_backups()
  expect_true(any(grepl("homework_-1", backups2)))
  expect_equal(length(backups2), length(backups)) # should be same number of backups
  expect_failure(expect_equal(backups2, backups)) # but different names
  
  # cleanup
  unlink(test_root_path, recursive = TRUE)
  backup_dir <- file.path(dirname(get_class_material_root_path()), "backup")
  if (dir.exists(backup_dir)) {
    unlink(backup_dir, recursive = TRUE)
  }
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  unlink(config_dir, recursive = TRUE)

})





test_that("can backup the whole sds1000_class_materials directory to a zip file", {
  
  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()

  # download a test directory to have something to backup
  download_github_directory("homework/homework_-1")
  
  # save a zip of all class materials
  save_zip_of_all_class_material()
  
  # check that the zip file exists
  zip_file_path <- file.path(get_class_material_root_path(), "backup")
  expect_true(grepl("all_sds1000_class_material.*", 
                    list.files(zip_file_path, pattern = "\\.zip$", full.names = TRUE)))

  # cleanup
  setwd(original_wd)
  unlink(test_root_path, recursive = TRUE)

  
})




test_that("can move the sds1000_class_material/ directory when changing the root dir path", {
  
  # set the root path to a temporary directory
  test_root_path <- setup_test_root_path()

  # download a test directory to have something to move
  download_github_directory("homework/homework_-1")
  
  # move the class material root path to a new location
  new_test_root_path <- file.path(tempdir(), "new_root")
  set_class_material_root_path(new_test_root_path)
  
  # check that the new root path is set correctly
  expect_equal(get_class_material_root_path(), file.path(new_test_root_path, "sds1000_material"))
  
  # check that the old directory is moved to the new location
  expected_dir <- file.path(new_test_root_path, "sds1000_material", "homework", "homework_-1")
  expect_true(dir.exists(expected_dir))
  
  # check for a specific file in the moved directory
  expected_file <- file.path(expected_dir, "homework_-1.qmd")
  expect_true(file.exists(expected_file))
  
  # check that old sds1000_material directory is removed
  old_materials_path <- file.path(test_root_path, "sds1000_material")
  expect_false(dir.exists(old_materials_path))
  
  # cleanup
  setwd(original_wd)
  unlink(test_root_path, recursive = TRUE)
  unlink(new_test_root_path, recursive = TRUE)
  
  
})
          


# move RStudio back to display the original working directory
rstudioapi::filesPaneNavigate(original_wd)

