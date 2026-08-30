# Runs before any of the tests, and cleans up after all of them have finished
#
# The tests point the package at temporary directories, which overwrites the
# configuration file that records where the person running the tests keeps
# their own class material. Without this, running the tests would leave that
# configuration pointing at a temporary directory that no longer exists, and
# goto_homework() and friends would stop working for them afterwards.

original_config_file <- file.path(tools::R_user_dir("SDS1000", which = "config"),
                                  "root_path.txt")

original_root_path <- if (file.exists(original_config_file)) {
  readLines(original_config_file, n = 1)
} else {
  NULL
}

original_working_directory <- getwd()


withr::defer({

  # several of the functions in the package change the working directory
  setwd(original_working_directory)

  if (is.null(original_root_path)) {

    unlink(original_config_file)

  } else {

    if (!dir.exists(dirname(original_config_file))) {
      dir.create(dirname(original_config_file), recursive = TRUE)
    }

    writeLines(original_root_path, original_config_file)

  }

}, teardown_env())
