# Tests for the functions that back up, restore and zip class material
#
# These tests build fake class material in a temporary directory rather than
# downloading anything, so they do not need a network connection.


# zipping needs a zip program to be available, which is not the case on every
# machine that the package might be tested on
skip_if_no_zip_program <- function() {

  zip_program <- Sys.getenv("R_ZIPCMD", "zip")

  if (!nzchar(Sys.which(zip_program))) {
    skip("no zip program is available")
  }

}




test_that("expand_folder_name() builds the folder name for numbered material", {

  expect_equal(expand_folder_name("homework", 1),
               file.path("homework", "homework_01"))

  expect_equal(expand_folder_name("homework", -1),
               file.path("homework", "homework_-1"))

  expect_equal(expand_folder_name("practice_session", 3),
               file.path("practice_session", "practice_session_03"))

  expect_equal(expand_folder_name("class_code", 12),
               file.path("class_code", "class_12"))

})




test_that("expand_folder_name() ignores the number for material that is not numbered", {

  expect_equal(expand_folder_name("final_project", NULL), "final_project")
  expect_equal(expand_folder_name("other", NULL), "other")

})




test_that("move_to_backup() only accepts the main class material directories", {

  local_test_root_path()

  expect_error(move_to_backup("not_a_real_directory", 1),
               "Invalid folder name")

  # the error message should list the directories that can be used
  expect_error(move_to_backup("homeworks", 1), "homework")

})




test_that("list_backups() returns nothing when no backups have been made", {

  local_test_root_path()

  expect_length(list_backups(), 0)
  expect_true(is.character(list_backups()))

})




test_that("move_to_backup() zips a directory up and empties it out", {

  skip_if_no_zip_program()

  local_test_root_path()

  dir_to_backup <- create_fake_class_material("homework/homework_01",
                                              c("homework_01.qmd", "data.csv"))

  move_to_backup("homework", 1)

  # the original files should no longer be there
  expect_length(list.files(dir_to_backup), 0)

  # and there should now be a backup with the directory name in it
  the_backups <- list_backups()
  expect_length(the_backups, 1)
  expect_true(grepl("homework_01", the_backups[1]))
  expect_true(grepl("\\.zip$", the_backups[1]))

})




test_that("restore_from_backup() puts the backed up files back", {

  skip_if_no_zip_program()

  local_test_root_path()

  dir_to_backup <- create_fake_class_material("homework/homework_01",
                                              c("homework_01.qmd", "data.csv"))
  writeLines("my own work", file.path(dir_to_backup, "my_answers.qmd"))

  move_to_backup("homework", 1)

  restore_from_backup(list_backups()[1])

  expect_true(dir.exists(dir_to_backup))
  expect_setequal(list.files(dir_to_backup),
                  c("homework_01.qmd", "data.csv", "my_answers.qmd"))

  expect_equal(readLines(file.path(dir_to_backup, "my_answers.qmd")), "my own work")

})




test_that("restore_from_backup() backs up what is already there before restoring", {

  skip_if_no_zip_program()

  local_test_root_path()

  dir_to_backup <- create_fake_class_material("homework/homework_01", "homework_01.qmd")
  writeLines("the first version", file.path(dir_to_backup, "homework_01.qmd"))

  move_to_backup("homework", 1)
  the_first_backup <- list_backups()[1]

  # backups are named using a timestamp that is only accurate to the second, so
  # two backups of the same directory made within the same second end up with
  # the same file name. Waiting here keeps the two backups in this test apart.
  Sys.sleep(1.1)

  # download a fresh copy of the homework
  create_fake_class_material("homework/homework_01", "homework_01.qmd")

  restore_from_backup(the_first_backup)

  # the restored files should be the first version again
  expect_equal(readLines(file.path(dir_to_backup, "homework_01.qmd")), "the first version")

  # and the fresh copy should have been backed up rather than thrown away
  the_backups <- list_backups()
  expect_length(the_backups, 1)
  expect_true(grepl("homework_01", the_backups[1]))
  expect_false(identical(the_backups[1], the_first_backup))

})




test_that("restore_from_backup() can keep the backup zip file", {

  skip_if_no_zip_program()

  local_test_root_path()

  create_fake_class_material("homework/homework_01", "homework_01.qmd")
  move_to_backup("homework", 1)

  the_backup <- list_backups()[1]

  restore_from_backup(the_backup, delete_backup_zip_file = FALSE)

  # the backup zip file should still be listed after restoring from it
  expect_true(the_backup %in% list_backups())

})




test_that("restore_from_backup() gives an error for a backup file that does not exist", {

  local_test_root_path()

  expect_error(restore_from_backup("homework/homework_99_20250817_215216.zip"),
               "Backup file does not exist")

})




test_that("save_zip_of_all_class_material() creates a zip file of everything", {

  skip_if_no_zip_program()

  local_test_root_path()

  create_fake_class_material("homework/homework_01", "homework_01.qmd")
  create_fake_class_material("class_code/class_01", "class_01.qmd")

  save_zip_of_all_class_material()

  backup_dir <- file.path(get_class_material_root_path(), "backup")
  the_zip_files <- list.files(backup_dir, pattern = "\\.zip$")

  expect_length(the_zip_files, 1)
  expect_true(grepl("^all_sds1000_class_material_", the_zip_files[1]))

  # the zip file should have both of the directories in it
  the_zipped_file_names <- utils::unzip(file.path(backup_dir, the_zip_files[1]), list = TRUE)$Name

  expect_true(any(grepl("homework_01.qmd", the_zipped_file_names)))
  expect_true(any(grepl("class_01.qmd", the_zipped_file_names)))

})




test_that("move_to_backup_short_path() backs up a directory given by its short path", {

  skip_if_no_zip_program()

  local_test_root_path()

  dir_to_backup <- create_fake_class_material("practice_session/practice_session_01",
                                              "practice_session_01.qmd")

  # the backup directory has to exist before a zip file can be written into it
  dir.create(file.path(get_class_material_root_path(), "backup", "practice_session"),
             recursive = TRUE)

  move_to_backup_short_path("practice_session/practice_session_01")

  expect_length(list.files(dir_to_backup), 0)

  the_backups <- list_backups()
  expect_length(the_backups, 1)
  expect_true(grepl("practice_session_01", the_backups[1]))

  # the backup file name ends in a timestamp
  expect_true(grepl("_[0-9]{8}_[0-9]{6}[.]zip$", the_backups[1]))

})
