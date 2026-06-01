# Package index

## Analyze Data

Functions for statistical analysis, simulation, and visualization.

- [`do_it()`](https://emeyers.github.io/SDS1000/reference/do_it.md) :
  Repeats a process many times and returns a vector of results
- [`get_proportion()`](https://emeyers.github.io/SDS1000/reference/get_proportion.md)
  : Calculates proportion of values in a particular category
- [`get_MAD_stat()`](https://emeyers.github.io/SDS1000/reference/get_MAD_stat.md)
  : Calculates the MAD statistic
- [`get_F_stat()`](https://emeyers.github.io/SDS1000/reference/get_F_stat.md)
  : Calculates the F statistic
- [`get_chisqr_stat()`](https://emeyers.github.io/SDS1000/reference/get_chisqr_stat.md)
  : Calculates the chi-square statistic
- [`stats_by_group()`](https://emeyers.github.io/SDS1000/reference/stats_by_group.md)
  : Statistics by group
- [`plot_norm()`](https://emeyers.github.io/SDS1000/reference/plot_norm.md)
  : Plots a Normal distribution
- [`plot_t()`](https://emeyers.github.io/SDS1000/reference/plot_t.md) :
  Plots a t-distribution
- [`plot_chisq()`](https://emeyers.github.io/SDS1000/reference/plot_chisq.md)
  : Plots a chi-squared distribution
- [`plot_f()`](https://emeyers.github.io/SDS1000/reference/plot_f.md) :
  Plots an F-distribution
- [`shuffle()`](https://emeyers.github.io/SDS1000/reference/shuffle.md)
  : Shuffles the order of the values in a vector
- [`ptail()`](https://emeyers.github.io/SDS1000/reference/ptail.md) :
  Calculates the proportion of points in a vector more extreme than a
  specified value
- [`resample_pairs()`](https://emeyers.github.io/SDS1000/reference/resample_pairs.md)
  : Samples with replacement the same rows from two vectors
- [`cnorm()`](https://emeyers.github.io/SDS1000/reference/cnorm.md) :
  Returns the critical value(s) from the normal distribution
- [`ct()`](https://emeyers.github.io/SDS1000/reference/ct.md) : Returns
  the critical value(s) from a t-distribution
- [`rsprinkles()`](https://emeyers.github.io/SDS1000/reference/rsprinkles.md)
  : Generate a random sample of sprinkle colors
- [`rapprovals()`](https://emeyers.github.io/SDS1000/reference/rapprovals.md)
  : Generate a random sample of a president's approval ratings
- [`rflip()`](https://emeyers.github.io/SDS1000/reference/rflip.md) :
  Gives a count/proportion from simulating n coin flips
- [`rroll()`](https://emeyers.github.io/SDS1000/reference/rroll.md) :
  Generates a vector of random die rolls

## Get Class Material

Functions for navigating to homework, class sessions, and other course
material.

- [`goto_homework()`](https://emeyers.github.io/SDS1000/reference/goto_homework.md)
  : Go to the directory for specific homework
- [`goto_class()`](https://emeyers.github.io/SDS1000/reference/goto_class.md)
  : Go to the directory for a specific class
- [`goto_practice_session()`](https://emeyers.github.io/SDS1000/reference/goto_practice_session.md)
  : Go to the directory for a specific practice session
- [`goto_final_project()`](https://emeyers.github.io/SDS1000/reference/goto_final_project.md)
  : Go to the directory for the final project
- [`goto_directory()`](https://emeyers.github.io/SDS1000/reference/goto_directory.md)
  : Sets the working directory and has RStudio file pane set to that
  directory If the directory does not exist, the user is prompted to
  download the files.

## Backup Data, and Setup/Configuration

Functions for configuration and backing up class material.

- [`save_zip_of_all_class_material()`](https://emeyers.github.io/SDS1000/reference/save_zip_of_all_class_material.md)
  : Save a zip file of all class material
- [`get_version()`](https://emeyers.github.io/SDS1000/reference/get_version.md)
  : Get the package name and version number

## Deprecated or package management

Functions that have been deprecated or should only be used rarely.

- [`pnull()`](https://emeyers.github.io/SDS1000/reference/pnull.md) :
  Calculates the proportion of points in a vector greater than a
  specified value
- [`rflip_count()`](https://emeyers.github.io/SDS1000/reference/rflip_count.md)
  : Gives a count/proportion from simulating n coin flips
- [`get_sprinkle_sample()`](https://emeyers.github.io/SDS1000/reference/get_sprinkle_sample.md)
  : Generate a random sample of sprinkle colors
- [`get_approval_sample()`](https://emeyers.github.io/SDS1000/reference/get_approval_sample.md)
  : Generate a random sample of a president's approval ratings
- [`rflip_sequence()`](https://emeyers.github.io/SDS1000/reference/rflip_sequence.md)
  : Generates a sequence of coin flips
- [`download_github_directory()`](https://emeyers.github.io/SDS1000/reference/download_github_directory.md)
  : Download files from a GitHub directory
- [`download_any_file()`](https://emeyers.github.io/SDS1000/reference/download_any_file.md)
  : Download any file related to the class
- [`download_only_if_missing()`](https://emeyers.github.io/SDS1000/reference/download_only_if_missing.md)
  : A helper function that downloads a file only if it does not already
  exist. If the file already exists, a message is shown (if desired).
- [`list_github_files()`](https://emeyers.github.io/SDS1000/reference/list_github_files.md)
  : Lists the files that are on the class GitHub site in a particular
  directory
- [`move_to_backup()`](https://emeyers.github.io/SDS1000/reference/move_to_backup.md)
  : Move a folder to the backup directory
- [`restore_from_backup()`](https://emeyers.github.io/SDS1000/reference/restore_from_backup.md)
  : Restore a backup from a zip file
- [`list_backups()`](https://emeyers.github.io/SDS1000/reference/list_backups.md)
  : List all available backups
- [`set_class_material_root_path()`](https://emeyers.github.io/SDS1000/reference/set_class_material_root_path.md)
  : Set the root path where class materials will be downloaded
- [`get_class_material_root_path()`](https://emeyers.github.io/SDS1000/reference/get_class_material_root_path.md)
  : Get the root path for class materials
- [`update_installed_packages()`](https://emeyers.github.io/SDS1000/reference/update_installed_packages.md)
  : Installs packages that are used in the class
