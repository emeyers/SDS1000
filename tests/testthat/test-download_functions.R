# Tests for the functions that download class material from GitHub
#
# Most of these tests replace the functions that actually reach out to the
# internet with stand-ins, so that the logic in the package can be tested
# quickly and without a network connection. The few tests that really do talk
# to GitHub are skipped when there is no internet connection available.


test_that("download_only_if_missing() does not download a file that already exists", {

  existing_file <- tempfile(fileext = ".qmd")
  writeLines("this file already exists", existing_file)
  on.exit(unlink(existing_file))

  # if download.file() gets called then the test fails
  testthat::local_mocked_bindings(
    download.file = function(...) stop("download.file() should not have been called"),
    .package = "utils"
  )

  expect_silent(download_only_if_missing(existing_file, "https://example.com/a_file.qmd"))

  # the existing file must be left alone
  expect_equal(readLines(existing_file), "this file already exists")

})




test_that("download_only_if_missing() can report that a file already exists", {

  existing_file <- tempfile(fileext = ".qmd")
  writeLines("this file already exists", existing_file)
  on.exit(unlink(existing_file))

  expect_message(
    download_only_if_missing(existing_file, "https://example.com/a_file.qmd",
                             show_existing_file_message = TRUE),
    "already exists"
  )

})




test_that("download_only_if_missing() downloads a file that does not exist yet", {

  missing_file <- file.path(tempdir(), "a_file_that_is_not_there.qmd")
  unlink(missing_file)
  on.exit(unlink(missing_file))

  downloaded_arguments <- NULL

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, ...) {
      downloaded_arguments <<- list(url = url, destfile = destfile, mode = mode)
      writeLines("downloaded contents", destfile)
      invisible(0)
    },
    .package = "utils"
  )

  download_only_if_missing(missing_file, "https://example.com/a_file.qmd", mode = "w")

  expect_equal(downloaded_arguments$url, "https://example.com/a_file.qmd")
  expect_equal(downloaded_arguments$destfile, missing_file)
  expect_equal(downloaded_arguments$mode, "w")
  expect_true(file.exists(missing_file))

})




test_that("download_only_if_missing() can report the file it is downloading", {

  missing_file <- file.path(tempdir(), "another_missing_file.qmd")
  unlink(missing_file)
  on.exit(unlink(missing_file))

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("downloaded contents", destfile)
      invisible(0)
    },
    .package = "utils"
  )

  expect_message(
    download_only_if_missing(missing_file, "https://example.com/a_file.qmd",
                             show_nonexistent_file_message = TRUE),
    "another_missing_file.qmd"
  )

})




test_that("download_any_file() builds the GitHub url from the class repository", {

  saved_wd <- getwd()
  download_location <- file.path(tempdir(), "download_any_file_test")
  dir.create(download_location, showWarnings = FALSE)
  setwd(download_location)
  on.exit({
    setwd(saved_wd)
    unlink(download_location, recursive = TRUE)
  })

  downloaded_arguments <- NULL

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, ...) {
      downloaded_arguments <<- list(url = url, destfile = destfile, mode = mode)
      writeLines("downloaded contents", destfile)
      invisible(0)
    },
    .package = "utils"
  )

  download_any_file("homework/homework_-1/homework_-1.qmd")

  expect_equal(downloaded_arguments$url,
               paste0(get_base_url(), "homework/homework_-1/homework_-1.qmd"))

  # the file is saved using only the file name, not the whole path
  expect_equal(downloaded_arguments$destfile, "homework_-1.qmd")
  expect_equal(downloaded_arguments$mode, "wb")
  expect_true(file.exists("homework_-1.qmd"))

})




test_that("download_any_file() only downloads again when force_download is TRUE", {

  saved_wd <- getwd()
  download_location <- file.path(tempdir(), "force_download_test")
  dir.create(download_location, showWarnings = FALSE)
  setwd(download_location)
  on.exit({
    setwd(saved_wd)
    unlink(download_location, recursive = TRUE)
  })

  writeLines("the original contents", "homework_-1.qmd")

  number_of_downloads <- 0

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      number_of_downloads <<- number_of_downloads + 1
      writeLines("the downloaded contents", destfile)
      invisible(0)
    },
    .package = "utils"
  )

  # the file already exists, so nothing should be downloaded
  download_any_file("homework/homework_-1/homework_-1.qmd")
  expect_equal(number_of_downloads, 0)
  expect_equal(readLines("homework_-1.qmd"), "the original contents")

  # forcing the download overwrites the file
  download_any_file("homework/homework_-1/homework_-1.qmd", force_download = TRUE)
  expect_equal(number_of_downloads, 1)
  expect_equal(readLines("homework_-1.qmd"), "the downloaded contents")

})




test_that("list_github_files() returns only the file names in the given directory", {

  # a stand-in for the listing of the whole repository that the GitHub api
  # returns, so that this test does not need a network connection
  fake_repository_tree <- list(
    tree = list(
      list(path = "README.md"),
      list(path = "ClassMaterial/homework/homework_-1/homework_-1.qmd"),
      list(path = "ClassMaterial/homework/homework_-1/test_data.rda"),
      list(path = "ClassMaterial/homework/homework_01/homework_01.qmd"),
      list(path = "ClassMaterial/class_code/class_-1/class_-1.qmd")
    )
  )

  testthat::local_mocked_bindings(
    GET = function(url, ...) fake_github_response(url),
    content = function(...) fake_repository_tree,
    .package = "httr"
  )

  the_file_names <- list_github_files("homework/homework_-1")

  expect_equal(the_file_names, c("homework_-1.qmd", "test_data.rda"))

  # a directory with nothing in it returns an empty vector
  expect_length(list_github_files("homework/homework_99"), 0)

})




test_that("list_github_files() asks GitHub about the right repository and branch", {

  requested_url <- NULL

  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      requested_url <<- url
      fake_github_response(url)
    },
    content = function(...) list(tree = list()),
    .package = "httr"
  )

  list_github_files("homework/homework_-1")

  expect_true(grepl(get_github_user_name(), requested_url, fixed = TRUE))
  expect_true(grepl(get_package_name(), requested_url, fixed = TRUE))
  expect_true(grepl(get_branch_name(), requested_url, fixed = TRUE))

})




test_that("download_github_directory() creates the directory and moves into it", {

  local_test_root_path()

  testthat::local_mocked_bindings(
    list_github_files = function(...) c("class_-1.qmd", "class_-1_data.rda"),
    download_only_if_missing = function(save_file_name, git_full_path, ...) {
      writeLines(paste("contents downloaded from", git_full_path), save_file_name)
    }
  )

  download_github_directory("class_code/class_-1")

  expected_dir <- file.path(get_class_material_root_path(), "class_code/class_-1")

  expect_true(dir.exists(expected_dir))
  expect_setequal(list.files(expected_dir), c("class_-1.qmd", "class_-1_data.rda"))

  # the working directory should now be the directory the files were saved in
  expect_equal(normalizePath(getwd()), normalizePath(expected_dir))

  # the files should have been downloaded from the class GitHub repository
  downloaded_contents <- readLines(file.path(expected_dir, "class_-1.qmd"))
  expect_true(grepl(get_base_url(), downloaded_contents, fixed = TRUE))

})




test_that("download_github_directory() gives a helpful error for a directory that is not on GitHub", {

  local_test_root_path()

  testthat::local_mocked_bindings(
    list_github_files = function(...) character(0)
  )

  expect_error(download_github_directory("homework/homework_99"),
               "does not exist in the class GitHub repository")

})




test_that("goto_directory() moves to a directory that already has all of its files", {

  local_test_root_path()

  create_fake_class_material("homework/homework_01", c("homework_01.qmd", "data.csv"))

  # menu() should never be called, because nothing needs to be downloaded
  testthat::local_mocked_bindings(
    list_github_files = function(...) c("homework_01.qmd", "data.csv")
  )
  testthat::local_mocked_bindings(
    menu = function(...) stop("menu() should not have been called"),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    filesPaneNavigate = function(...) invisible(NULL),
    .package = "rstudioapi"
  )

  goto_directory("homework/homework_01", "homework 1")

  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_01")
  expect_equal(normalizePath(getwd()), normalizePath(expected_dir))

})




test_that("goto_directory() offers to download a directory that is not there yet", {

  local_test_root_path()

  testthat::local_mocked_bindings(
    list_github_files = function(...) c("homework_01.qmd"),
    download_only_if_missing = function(save_file_name, git_full_path, ...) {
      writeLines("downloaded contents", save_file_name)
    }
  )

  # answering "Yes" to the prompt
  testthat::local_mocked_bindings(menu = function(...) 1L, .package = "utils")
  testthat::local_mocked_bindings(filesPaneNavigate = function(...) invisible(NULL),
                                  .package = "rstudioapi")

  expect_output(goto_directory("homework/homework_01", "homework 1"),
                "Downloading homework 1 files")

  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_01")
  expect_true(file.exists(file.path(expected_dir, "homework_01.qmd")))
  expect_equal(normalizePath(getwd()), normalizePath(expected_dir))

})




test_that("goto_directory() leaves the files alone when the download is declined", {

  local_test_root_path()

  testthat::local_mocked_bindings(
    list_github_files = function(...) c("homework_01.qmd"),
    download_only_if_missing = function(...) stop("nothing should have been downloaded")
  )

  # answering "No" to the prompt
  testthat::local_mocked_bindings(menu = function(...) 2L, .package = "utils")

  the_message <- goto_directory("homework/homework_01", "homework 1")

  expect_true(grepl("Not downloading files", the_message))

  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_01")
  expect_false(dir.exists(expected_dir))

})




test_that("goto_directory() explains itself when the prompt is cancelled", {

  local_test_root_path()

  testthat::local_mocked_bindings(
    list_github_files = function(...) c("homework_01.qmd")
  )

  # pressing enter at the prompt returns 0
  testthat::local_mocked_bindings(menu = function(...) 0L, .package = "utils")

  expect_true(grepl("must select 1 or 2",
                    goto_directory("homework/homework_01", "homework 1")))

})




test_that("goto_directory() offers to download files that are missing from a local directory", {

  local_test_root_path()

  # the local directory only has one of the two files that are on GitHub
  create_fake_class_material("homework/homework_01", "homework_01.qmd")

  testthat::local_mocked_bindings(
    list_github_files = function(...) c("homework_01.qmd", "homework_01_data.rda"),
    download_only_if_missing = function(save_file_name, git_full_path, ...) {
      if (!file.exists(save_file_name)) {
        writeLines("downloaded contents", save_file_name)
      }
    }
  )

  testthat::local_mocked_bindings(menu = function(...) 1L, .package = "utils")
  testthat::local_mocked_bindings(filesPaneNavigate = function(...) invisible(NULL),
                                  .package = "rstudioapi")

  expect_output(goto_directory("homework/homework_01", "homework 1"),
                "Downloading missing")

  expected_dir <- file.path(get_class_material_root_path(), "homework/homework_01")
  expect_setequal(list.files(expected_dir), c("homework_01.qmd", "homework_01_data.rda"))

})




test_that("the goto_* functions ask for the directories that are named after them", {

  requested_directories <- character(0)

  testthat::local_mocked_bindings(
    goto_directory = function(dir_path, download_message = "") {
      requested_directories <<- c(requested_directories, dir_path)
      invisible(NULL)
    }
  )

  goto_homework(1)
  goto_homework(-1)
  goto_class(2)
  goto_practice_session(3)
  goto_final_project()

  expect_equal(requested_directories,
               c("homework/homework_01",
                 "homework/homework_-1",
                 "class_code/class_02",
                 "practice_sessions/practice_session_03",
                 "final_project"))

})




test_that("the goto_* functions pass along a message describing what is downloaded", {

  requested_messages <- character(0)

  testthat::local_mocked_bindings(
    goto_directory = function(dir_path, download_message = "") {
      requested_messages <<- c(requested_messages, download_message)
      invisible(NULL)
    }
  )

  goto_homework(1)
  goto_class(2)
  goto_practice_session(3)
  goto_final_project()

  expect_equal(requested_messages,
               c("homework 1", "class 2", "practice session 3", "final project"))

})




test_that("list_github_files() finds the test homework on the real class GitHub site", {

  skip_on_cran()
  skip_if_offline()

  the_file_names <- list_github_files("homework/homework_-1")

  expect_gt(length(the_file_names), 0)
  expect_true("homework_-1.qmd" %in% the_file_names)

})
