
#' Download files from a GitHub directory
#'
#' This function downloads all files from a specified directory in the class
#' GitHub site
#'
#' @param github_directory_name A string indicating the name of the directory in
#'   the class GitHub repository
#'   
#' @examples
#'  # Download the first homework file
#'  \dontrun{download_github_directory("homework/homework_01")}
#'
#' @export
download_github_directory <- function(download_dir_name) {
  
  save_full_path <- file.path(get_save_path(), download_dir_name) 
  github_dir_name <- paste0(get_base_url(), download_dir_name)
  
  file_names <- list_class_files(download_dir_name)
  
  if (length(file_names) == 0) {
    stop(paste("The directory", github_dir_name, "does not exist in the class GitHub repository."))
  }
  
  # create the directory to save the homework files
  if (!dir.exists(save_full_path)) {
    dir.create(save_full_path, recursive = TRUE)
  }
  
  for (file_name in file_names) {
    
    git_full_path <- paste0(github_dir_name, "/", file_name)
    save_file_name <- file.path(save_full_path, file_name)
    
    # if a file doesn't exist download it, otherwise give a warning that it already
    #  exits and don't download it
    download_nonexistent_file(save_file_name, git_full_path, mode = "w")
    
  }
  
  # set the working directory to the directory where the files were downloaded
  setwd(save_full_path)
  
}





#' Download homework 
#'
#' This function downloads homework files from the class GitHub repository.
#'
#' @param homework_number A number indicating which homework number file to get.
#'
#' @examples
#'  # Download the first homework file
#'  \dontrun{download_homework(1)}
#'
#' @export
download_homework <- function(homework_number) {
  
  homework_dir_name <- paste0("homework/homework_", sprintf("%02d", homework_number))
  
  download_github_directory(homework_dir_name)
  
}




#' Download code related to the class
#'
#' This function downloads code that will be used in class from the class GitHub
#' repository.
#'
#' @param class_number A number indicating which class we should get the code
#'   for.
#'
#' @examples
#'  # Download code from the first class
#'  \dontrun{download_class_code(1)}
#'
#' @export
download_class_code <- function(class_number) {
  
  class_code_dir_name <- paste0("class_code/class_", sprintf("%02d", class_number))
  
  download_github_directory(class_code_dir_name)
  
}





#' Download material from a practice session
#'
#' This function downloads practice session material from the class GitHub
#' repository.
#'
#' @param session_number A number indicating which practice session we should get the code
#'   for.
#'
#' @examples
#'  # Download code from the first practice session
#'  \dontrun{download_practice_session(1)}
#'
#' @export
download_practice_session <- function(session_number) {
  
  practice_session_name <- paste0("practice_sessions/practice_session_", 
                                  sprintf("%02d", session_number))
  
  download_github_directory(practice_session_name)
  
}





#' Download any file related to the class
#'
#' This function downloads any file related to the class from the class GitHub
#' repository.
#'
#' @param file_path_and_name The name of the path and file to download.
#'
#' @param force_download Will download and overwrite an existing file if the
#' downloaded file has the same name.
#' 
#' @param mode A character indicating the mode with which to write the file.
#'   Useful values are "w", "wb" (binary), "a" (append) and "ab". Not used for
#'   methods "wget" and "curl". See also ‘Details’, notably about using "wb" for
#'   Windows. See download.file() mode argument.
#'
#' @examples
#'  # Download an image from the class GitHub repository
#'  \dontrun{download_any_file("homework/homework_-1/homework_-1.Rmd")}
#'
#' @export
download_any_file <- function(file_path_and_name, force_download = FALSE, mode = "wb"){
  
  full_path <- paste0(get_base_url(), file_path_and_name)
  file_name <- basename(file_path_and_name)
  file_dir_name <- dirname(file_path_and_name)
  
  # check the file exists on GitHub and if not throw and error
  #check_github_file_exists(file_dir_name, file_name)
  
  # only download the file if it doesn't exist or if force_download is TRUE
  if (!file.exists(file_name) || force_download == TRUE) {
    utils::download.file(full_path, file_name, mode = mode)
  }
  
}





### Helper functions -----------------------------------------------------



# A helper function that downloads a file if it does not already exist. 
# Otherwise it gives a warning that the file already exists and does not download it. 

download_nonexistent_file <- function(save_file_name, git_full_path, mode = "wb") {
  

  if (file.exists(save_file_name)) {
 
    file_exists_message <- paste("The class code file you are trying to download", save_file_name,
                                 "already exists. Please rename the file", save_file_name,
                                 "and then rerun this function to download a new copy")
    
    warning(file_exists_message)
    
  } else {
    
    # if the file does not already exist, download it
    utils::download.file(git_full_path, save_file_name, mode = mode)
    
  }
  
}



