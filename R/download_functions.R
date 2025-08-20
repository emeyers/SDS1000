
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
download_github_directory <- function(download_dir_name,
                                      show_existing_file_message = FALSE,
                                      show_nonexistent_file_message = FALSE) {
  
  save_full_path <- file.path(get_class_material_root_path(), download_dir_name) 
  github_dir_name <- paste0(get_base_url(), download_dir_name)
  
  github_file_names <- list_github_files(download_dir_name)
  
  if (length(github_file_names) == 0) {
    stop(paste("\n\n The directory", download_dir_name, "does not exist in the class GitHub repository.\n",
               "\n Perhaps these files have not been uploaded yet by the instructor?\n",
               "\n Please check the class syllabus to make sure the files are available (e.g., homework is posted by class on Tuesday, etc.)\n",
               "\n If you think this is a real issue, please post to Ed Discussion to let the instructor know."))
  }
  
  # create the directory to save the homework files
  if (!dir.exists(save_full_path)) {
    dir.create(save_full_path, recursive = TRUE)
  }
  
  for (file_name in github_file_names) {
    
    git_full_path <- paste0(github_dir_name, "/", file_name)
    save_file_name <- file.path(save_full_path, file_name)
    
    # if a file doesn't exist download it
    download_only_if_missing(save_file_name, git_full_path, mode = "w",
                              show_existing_file_message = show_existing_file_message,
                              show_nonexistent_file_message = show_nonexistent_file_message)
    
  }
  
  # set the working directory to the directory where the files were downloaded
  setwd(save_full_path)
  
}





#' Sets the working directory and has RStudio file pane set to that directory
#' If the directory does not exist, the user is prompted to download the files.
#'
#' @param dir_path A string indicating the path to the directory relative to
#'   the base save path.
#' @param download_message A string that will display what files are being downloaded
#'   (e.g., "homework 1").
#'
#' @examples
#'  # Go to the first homework directory, downloading files if necessary
#'  \dontrun{goto_directory("homework/homework_01", "homework 1")}
#'
#' @export
goto_directory <- function(dir_path, download_message = "") {
  
  local_full_dir_path <- file.path(get_class_material_root_path(), dir_path)
  
  if (!dir.exists(local_full_dir_path)) {
    
    prompt_message <- paste("\nThe files for", download_message, 
                            "do not exist. Would you like to download the files now?")
    
    choice <- menu(c("Yes", "No"), graphics = FALSE, title = prompt_message)

    if (choice == 1) {
      
      print(paste0("Downloading ",  download_message, " files..."))
      download_github_directory(dir_path)

    } else if (choice == 2) { 
      return("Not downloading files. Please post to Ed Discussion if you need help.")

    } else {     # This happens if they select 0
      return("You must select 1 or 2. Please try again.")
    }
    
  }
  
  
  # If files are missing from a local directory that are on GitHub, download them
  github_files <- list_github_files(dir_path)
  local_files <- list.files(local_full_dir_path)
  
  if (length(setdiff(github_files, local_files)) != 0) {
    
    prompt_message <- paste("\nSome files for", download_message, 
                            "are missing. Would you like to download the missing files now?")
    
    choice <- menu(c("Yes", "No"), graphics = FALSE, title = prompt_message)
    
    if (choice == 1) {
      
      print(paste0("Downloading missing ",  download_message, "files..."))
      
      download_github_directory(dir_path, show_nonexistent_file_message = TRUE)
      
    } else if (choice == 2) { 
      print("Not downloading missing files. Please post to Ed Discussion if you need help.")
      
    } else {     # This happens if they select 0
      return("You must select 1 or 2. Please try again.")
    }
    
  }
      

  # set the working directory to the directory where the files were downloaded
  # set RStudio file pane to that directory
  setwd(local_full_dir_path)
  rstudioapi::filesPaneNavigate(getwd())
  
}





#' Go to the directory for specific homework 
#'
#' This function takes one to the directory of a specific homework. If the 
#' homework has not been downloaded yet, the user is prompted to download it from
#' the class GitHub repository.
#'
#' @param homework_number A number indicating which homework directory to go to.
#'
#' @examples
#'  # Download the first homework file
#'  \dontrun{goto_homework(1)}
#'
#' @export
goto_homework <- function(homework_number) {
  
  homework_dir_name <- paste0("homework/homework_", sprintf("%02d", homework_number))
  download_message <- paste("homework", homework_number)
  
  goto_directory(homework_dir_name, download_message)
  
}
  




#' Go to the directory for a specific class 
#'
#' This function takes one to the directory of a specific class. If the 
#' class files have not been downloaded yet, the user is prompted to download them
#' from the class GitHub repository
#'
#' @param class_number A number indicating which class we should get the code
#'   for.
#'
#' @examples
#'  # Download code from the first class
#'  \dontrun{goto_class(1)}
#'
#' @export
goto_class <- function(class_number) {
  
  class_dir_name <- paste0("class_code/class_", sprintf("%02d", class_number))
  
  download_message <- paste("class", class_number)
  
  goto_directory(class_dir_name, download_message)
  
}





#' Go to the directory for a specific practice session 
#'
#' This function takes one to the directory of a specific practice session. If the 
#' practice ssion files have not been downloaded yet, the user is prompted to 
#' download them from the class GitHub repository
#'
#'
#' @param session_number A number indicating which practice session should go to
#'   for.
#'
#' @examples
#'  # Download code from the first practice session
#'  \dontrun{goto_practice_session(1)}
#'
#' @export
goto_practice_session <- function(session_number) {
  
  practice_session_name <- paste0("practice_sessions/practice_session_", 
                                  sprintf("%02d", session_number))
  
  download_message <- paste("practice session", session_number)
  
  goto_directory(practice_session_name, download_message)
  
}


#' Go to the directory for the final project
#'
#' This function takes one to the directory of the final project. If the
#' final project files have not been downloaded yet, the user is prompted to
#' download them from the class GitHub repository
#'
#' @examples
#'  # Download code from the final project
#'  \dontrun{goto_final_project()}
#'
#' @export
goto_final_project <- function() {
  
  final_project_dir_name <- "final_project"
  download_message <- "final project"
  
  goto_directory(final_project_dir_name, download_message)
  
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






#' A helper function that downloads a file only if it does not already exist.
#' If the file already exists, a message is shown (if desired).
#'
#' @param save_file_name The name of the file to save the downloaded file as.
#' @param git_full_path The full path to the file on GitHub.
#' @param mode A character indicating the mode with which to write the file.
#'   Useful values are "w", "wb" (binary), "a" (append) and "ab". Not used for
#'   methods "wget" and "curl". See also ‘Details’, notably about using "wb" for
#'   Windows. See download.file() mode argument.
#' @param show_existing_file_message A logical indicating whether to show a
#'   warning message if the file already exists.
#' @param show_nonexistent_file_message A logical indicating whether to show a
#'   message when downloading a file that does not already exist.
#'
#' @examples
#'  # Download a file if it doesn't already exist
#'  \dontrun{download_only_if_missing("homework_-1.Rmd",
#'  "https://raw.githubusercontent.com/your_github_username/your_repo_name/main/ClassMaterial/homework/homework_-1/homework_-1.Rmd")}
#'
#' @export
download_only_if_missing <- function(save_file_name, git_full_path, mode = "wb",
                                      show_existing_file_message = FALSE,
                                      show_nonexistent_file_message = FALSE) {
  

  if (file.exists(save_file_name)) {
 
    file_exists_message <- paste("The class code file you are trying to download", basename(save_file_name),
                                 "already exists. Please rename the file", basename(save_file_name),
                                 "and then rerun this function to download a new copy")
    
    if (show_existing_file_message) {
      message(file_exists_message)
    }
    
  } else {
    
    if (show_nonexistent_file_message) {
      message(paste("Downloading file:", basename(save_file_name)))
    }
    
    # if the file does not already exist, download it
    utils::download.file(git_full_path, save_file_name, mode = mode)
    
  }
  
}





#' Lists the files that are on the class GitHub site in a particular directory
#'
#' @param file_dir_name The name of a directory in the GitHub repository.
#'
#' @examples
#'  # List files in the data directory on GitHub 
#'  \dontrun{list_class_files("homework/homework_-1")}
#'
#' @export
list_github_files <- function(file_dir_name) {
  
  # newer repositories use the term "main" for the default repository branch
  file_url <- paste0("https://api.github.com/repos/", get_github_user_name(), "/",
                     get_package_name(), "/git/trees/", get_branch_name(), "?recursive=1")
  
  
  req <- httr::GET(file_url)
  stop_for_status(req)
  
  
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  file_names <- grep(paste0("ClassMaterial/", file_dir_name, "/"),
                     filelist, value = TRUE, fixed = TRUE)
  
  basename(file_names)
  
}







# # Helper function that checks if a given file exists in the file_dir_name on the
# # class github page. 
# 
# # Example: check_github_file_exists("homework/homework_-1", "test_data.rda")
# 
# check_github_file_exists <- function(file_dir_name, file_name) {
#   
#   file_names <- list_class_files(file_dir_name)
#   
#   if (!(file_name %in% file_names)) {
#     stop(paste("The file",  file_name,
#                "does not exit in the class GitHub repository."))
#   }
#   
# }



