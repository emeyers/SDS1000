# Helper functions that are shared across the SDS1000 test files


# The package remembers where the class material lives in a configuration file
# in the user's R config directory. Tests should never clobber the path that the
# person running the tests is actually using, so this helper stashes the current
# configuration, points the package at a fresh temporary root directory, and
# then puts the original configuration (and working directory) back when the
# calling test_that() block finishes.
#
# Returns the temporary root path that was set.

local_test_root_path <- function(envir = parent.frame()) {

  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  config_file <- file.path(config_dir, "root_path.txt")

  saved_config <- if (file.exists(config_file)) readLines(config_file, n = 1) else NULL
  saved_wd <- getwd()

  # start from a clean configuration so that set_class_material_root_path()
  # does not try to move any real class material around
  unlink(config_file)

  test_root_path <- file.path(tempdir(),
                              paste0("sds1000_test_root_",
                                     paste(sample(letters, 8, replace = TRUE), collapse = "")))
  dir.create(test_root_path, recursive = TRUE)

  suppressMessages(set_class_material_root_path(test_root_path))

  withr::defer({

    setwd(saved_wd)
    unlink(test_root_path, recursive = TRUE)

    if (is.null(saved_config)) {
      unlink(config_file)
    } else {
      if (!dir.exists(config_dir)) {
        dir.create(config_dir, recursive = TRUE)
      }
      writeLines(saved_config, config_file)
    }

  }, envir = envir)

  test_root_path

}




# Creates a directory of fake class material inside the class material root
# path, so that the backup/zip functions have something to work with without
# needing to download anything from GitHub.
#
# Returns the full path to the directory that was created.

create_fake_class_material <- function(short_dir_name, file_names = c("notes.qmd", "data.csv")) {

  full_dir_path <- file.path(get_class_material_root_path(), short_dir_name)
  dir.create(full_dir_path, recursive = TRUE, showWarnings = FALSE)

  for (file_name in file_names) {
    writeLines(paste("contents of", file_name), file.path(full_dir_path, file_name))
  }

  full_dir_path

}




# Opens a throw-away graphics device so that the plot_*() functions can be
# tested without any plots appearing on screen.

local_null_graphics_device <- function(envir = parent.frame()) {

  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)

  withr::defer({
    grDevices::dev.off()
    unlink(pdf_file)
  }, envir = envir)

  invisible(pdf_file)

}




# The lifecycle package only warns about soft-deprecated functions in some
# situations, so this helper turns the warnings on for the duration of a test.

local_lifecycle_warnings <- function(envir = parent.frame()) {

  old_options <- options(lifecycle_verbosity = "warning")

  withr::defer(options(old_options), envir = envir)

  invisible(NULL)

}




# Builds a minimal stand-in for the response object that httr::GET() returns,
# so that the download functions can be tested without a network connection.
# The object needs to be a real httr "response" so that httr::stop_for_status()
# knows what to do with it.

fake_github_response <- function(url = "", status_code = 200L) {

  structure(list(url = url, status_code = status_code), class = "response")

}




# Returns a copy of a package function that looks up the given replacement
# functions before anything else.
#
# testthat's mocking can only replace functions that live in a package's own
# namespace, so it cannot stand in for base R functions such as readline() that
# a package function calls. Putting the replacements in an environment between
# the function and the package namespace does work for those.

with_replacement_functions <- function(the_function, replacements) {

  replacement_env <- list2env(replacements, parent = environment(the_function))

  environment(the_function) <- replacement_env

  the_function

}
