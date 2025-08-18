# # Download files from a GitHub directory
# #
# # This function downloads all files from a specified directory in the class
# # GitHub site
# #
# # @param download_dir_name A string indicating the name of the directory in
# #   the class GitHub repository.
# # @param show_existing_file_message A logical indicating whether to show a
# #   message if a file already exists and is not being re-downloaded.
# # @param show_nonexistent_file_message A logical indicating whether to show a
# #   message when downloading a file that does not already exist.
# # @param force_redownload A logical indicating whether to force a re-download
# #  of files that already exist.
# #
# # @examples
# #  # Download the first homework file
# #  \dontrun{download_github_directory("homework/homework_01")}
# #
# # @export
# download_github_directory <- function(download_dir_name,
#                                       show_existing_file_message = FALSE,
#                                       show_nonexistent_file_message = FALSE,
#                                       force_redownload = FALSE) {
#
#   save_full_path <- file.path(get_class_material_root_path(), download_dir_name)
#   github_dir_name <- paste0(get_base_url(), download_dir_name)
#
#   github_file_names <- list_github_files(download_dir_name)
#
#   if (length(github_file_names) == 0) {
#     stop(paste("The directory", github_dir_name, "does not exist in the class GitHub repository."))
#   }
#
#   # create the directory to save the homework files
#   if (!dir.exists(save_full_path)) {
#     dir.create(save_full_path, recursive = TRUE)
#   }
#
#   for (file_name in github_file_names) {
#
#     git_full_path <- paste0(github_dir_name, "/", file_name)
#     save_file_name <- file.path(save_full_path, file_name)
#
#     # if a file doesn't exist download it
#     download_only_if_missing(save_file_name, git_full_path, mode = "w",
#                               show_existing_file_message = show_existing_file_message,
#                               show_nonexistent_file_message = show_nonexistent_file_message,
#                               force_redownload = force_redownload)
#
#   }
#
#   return(save_full_path)
#
# }
#
#
# # Navigates to a directory and downloads files if they are missing.
# #
# # This function checks if a directory exists locally, and if not, downloads it.
# # It also checks for missing files and downloads them. It then returns the path
# # to the directory.
# #
# # @param dir_path A string indicating the path to the directory relative to
# #   the base save path.
# # @param download_message A string that will display what files are being downloaded
# #   (e.g., "homework 1").
# #
# # @return The path to the local directory.
# #
# # @examples
# #  # Go to the first homework directory, downloading files if necessary
# #  \dontrun{goto_directory("homework/homework_01", "homework 1")}
# #
# # @export
# goto_directory <- function(dir_path, download_message = "") {
#
#   local_full_dir_path <- file.path(get_class_material_root_path(), dir_path)
#
#   if (!dir.exists(local_full_dir_path)) {
#     message("Directory not found locally. Downloading files for ", download_message, "...")
#     download_github_directory(dir_path, show_nonexistent_file_message = TRUE)
#   }
#
#   # If files are missing from a local directory that are on GitHub, download them
#   github_files <- list_github_files(dir_path)
#   local_files <- list.files(local_full_dir_path)
#
#   if (length(setdiff(github_files, local_files)) != 0) {
#     message("Missing files detected. Downloading missing files for ", download_message, "...")
#     download_github_directory(dir_path, show_nonexistent_file_message = TRUE)
#   }
#
#   message("Files are located in: ", local_full_dir_path)
#   message("You can set your working directory to this path using setwd()")
#
#   return(local_full_dir_path)
# }
