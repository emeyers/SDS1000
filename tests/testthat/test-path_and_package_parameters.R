# Tests for the functions that report the package parameters and that keep
# track of where the class material is stored


test_that("get_version() reports the package name and the installed version", {

  the_version <- get_version()

  expect_true(is.character(the_version))
  expect_length(the_version, 1)

  expect_equal(the_version,
               paste0("SDS1000: version ", utils::packageVersion("SDS1000")))

})




test_that("the package parameters point at the class GitHub repository", {

  expect_equal(get_package_name(), "SDS1000")
  expect_equal(get_github_user_name(), "emeyers")

  expect_true(is.character(get_branch_name()))
  expect_length(get_branch_name(), 1)

})




test_that("get_base_url() builds a raw GitHub url for the ClassMaterial directory", {

  the_base_url <- get_base_url()

  expect_equal(the_base_url,
               paste0("https://raw.githubusercontent.com/",
                      get_github_user_name(), "/", get_package_name(), "/",
                      get_branch_name(), "/ClassMaterial/"))

  # the url has to end in a slash so that file names can be pasted onto it
  expect_true(endsWith(the_base_url, "/"))

})




test_that("get_main_directory_names() lists the directories the class material is organized into", {

  the_directory_names <- get_main_directory_names()

  expect_true(is.character(the_directory_names))
  expect_setequal(the_directory_names,
                  c("homework", "class_code", "practice_session",
                    "final_project", "other"))

})




test_that("the poll parameters are set", {

  expect_true(is.character(get_poll_script_url()))
  expect_true(startsWith(get_poll_script_url(), "https://"))

  expect_true(is.character(get_poll_sheet_id()))
  expect_true(nzchar(get_poll_sheet_id()))

})




test_that("set_class_material_root_path() puts sds1000_material inside the root path", {

  test_root_path <- local_test_root_path()

  expect_equal(get_class_material_root_path(),
               file.path(test_root_path, "sds1000_material"))

})




test_that("set_class_material_root_path() returns the path it was given invisibly", {

  local_test_root_path()

  another_root_path <- file.path(tempdir(), "another_root")

  expect_invisible(suppressMessages(set_class_material_root_path(another_root_path)))
  expect_equal(suppressMessages(set_class_material_root_path(another_root_path)),
               another_root_path)

  unlink(another_root_path, recursive = TRUE)

})




test_that("set_class_material_root_path() says so when the path is already set", {

  test_root_path <- local_test_root_path()

  expect_message(set_class_material_root_path(test_root_path),
                 "already set to")

  # the path should be unchanged
  expect_equal(get_class_material_root_path(),
               file.path(test_root_path, "sds1000_material"))

})




test_that("set_class_material_root_path() remembers the path across R sessions", {

  test_root_path <- local_test_root_path()

  # the path is stored in a configuration file rather than in the R session, so
  # reading the file back gives the path that a later R session would find
  config_file <- file.path(tools::R_user_dir("SDS1000", which = "config"),
                           "root_path.txt")

  expect_true(file.exists(config_file))
  expect_equal(readLines(config_file, n = 1), test_root_path)

})




test_that("set_class_material_root_path() moves existing class material to the new root path", {

  local_test_root_path()

  create_fake_class_material("homework/homework_01", c("homework_01.qmd", "data.csv"))

  new_root_path <- file.path(tempdir(), "moved_root")
  on.exit(unlink(new_root_path, recursive = TRUE))

  old_materials_path <- get_class_material_root_path()

  suppressMessages(set_class_material_root_path(new_root_path))

  expect_equal(get_class_material_root_path(),
               file.path(new_root_path, "sds1000_material"))

  moved_dir <- file.path(new_root_path, "sds1000_material", "homework", "homework_01")
  expect_true(dir.exists(moved_dir))
  expect_setequal(list.files(moved_dir), c("homework_01.qmd", "data.csv"))

  # the class material should no longer be in the old location
  expect_false(dir.exists(old_materials_path))

})




test_that("get_class_material_root_path() gives back a path even when nothing has been set", {

  local_test_root_path()

  # remove the configuration so that the default location has to be worked out
  config_file <- file.path(tools::R_user_dir("SDS1000", which = "config"),
                           "root_path.txt")
  unlink(config_file)

  expect_message(the_root_path <- get_class_material_root_path(),
                 "Root path not set")

  expect_true(is.character(the_root_path))
  expect_true(endsWith(the_root_path, "sds1000_material"))

})
