
#' Move a folder to the backup directory
#' 
#' This function moves a specified folder to a backup directory and zips it with a timestamp.
#' #' @param folder_root_name The name of the folder to be backed up. The folder must be one of the following:
#' #' "homework", "class_code", "practice_session", "final_project", or "other".
#' #' @param n An integer parameter that specifies the homework, class, or practice session number. 
#' For final projects, this parameter is not used.
#' #' @return NULL
#' #' @examples
#' #' \dontrun{move_to_backup("homework", 1)}
#' @export
#' 

move_to_backup <- function(folder_root_name, n = NULL) {
  
  
  # Check that the folder name is valid  
  if(!(folder_root_name %in% get_main_directory_names())) {
    stop(paste0("Invalid folder name, please use one of the following: ", 
               "'", paste0(get_main_directory_names(), collapse = "', '"), "'"))
  }
  
  
  # Create a backup folder if it doesn't exist
  backup_folder_full_path <- file.path(get_class_material_root_path(), "backup")
  if (!dir.exists(file.path(backup_folder_full_path, folder_root_name))) {
    dir.create(file.path(backup_folder_full_path, folder_root_name), recursive = TRUE)
  }
  
  
  # Expand to the full folder names
  expanded_folder_name <- expand_folder_name(folder_root_name, n)
  
  move_to_backup_short_path(expanded_folder_name)
  
  
  
}






# helper function to move to backup given a short path name, e.g.,
# move_to_backup_short_path("homework/homework_01")

move_to_backup_short_path <- function(expanded_folder_name) {
  
  # Create a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create the new folder name with timestamp
  backup_folder_name <- paste0(expanded_folder_name, "_", timestamp)
  
  backup_folder_path <- file.path(get_class_material_root_path(), "backup")
  
  # Zip the folder
  zip_file_name <- paste0(backup_folder_name, ".zip")
  zip_file_path <- file.path(backup_folder_path, zip_file_name)
  zip(zipfile = zip_file_path, files = file.path(get_class_material_root_path(), expanded_folder_name), 
      flags = "-r", extras = "-j")
  
  # Remove the original folder after zipping
  # Don't love this because it deletes files :/
  unlink(file.path(get_class_material_root_path(), expanded_folder_name), recursive = TRUE)
  
  
}





# helper function to expand the folder name based on the root name and number
expand_folder_name <- function(folder_root_name, n) {
  
  if (folder_root_name %in% c("homework", "practice_session")) {
    expanded_folder_name <- file.path(folder_root_name, paste0(folder_root_name, "_", sprintf("%02d", n))) 
    
  } else if (folder_root_name == "class_code") {
    expanded_folder_name <- file.path(folder_root_name, paste0("class", "_", sprintf("%02d", n))) 
    
  } else if (folder_root_name == "final_project") {
    expanded_folder_name <- folder_root_name
    
  } else if (folder_root_name == "other") { # not sure exactly what to do here
    expanded_folder_name <- folder_root_name
  }
  
  expanded_folder_name
  
}






#' Restore a backup from a zip file
#'
#' This function restores a backup from a specified zip file in the backup directory.
#' #' @param backup_file_name The name of the backup zip file to restore.
#' #' @return NULL
#' 
#' @examples
#' \dontrun{restore_from_backup("homework/homework_-1_20250817_215216.zip")}
#' @export

restore_from_backup <- function(backup_file_name, delete_backup_zip_file = TRUE) {
  
  restore_dir_name <- substr(backup_file_name, 1, nchar(backup_file_name) - 20)
  restore_full_path <- file.path(get_class_material_root_path(), restore_dir_name)
  
  if (dir.exists(restore_full_path)) {
    move_to_backup_short_path(restore_dir_name)
  }
  
  
  # Check if the backup file exists
  backup_folder_full_path <- file.path(get_class_material_root_path(), "backup")
  zip_file_path <- file.path(backup_folder_full_path, backup_file_name)
  
  if (!file.exists(zip_file_path)) {
    stop(paste("Backup file does not exist:", zip_file_path))
  }
  
  
  # Unzip the backup file to the class material root path
  unzip(zipfile = zip_file_path, exdir = file.path(get_class_material_root_path(), restore_dir_name))
  
  # Optionally, you can remove the zip file after restoring
  if (delete_backup_zip_file) {
    unlink(zip_file_path)
  }
  
  
}





#' List all available backups
#' 
#' This function lists all available backup zip files in the backup directory.
#' #' @return A character vector of backup zip file names.
#' #' @examples
#' #' \dontrun{list_backups()}
#' @export

list_backups <- function() {
  
  backup_folder_full_path <- file.path(get_class_material_root_path(), "backup")
  
  if (!dir.exists(backup_folder_full_path)) {
    return(character(0))
  }
  
  list.files(backup_folder_full_path, recursive = TRUE, pattern = "\\.zip$", full.names = FALSE)
  
}





#' Save a zip file of all class material
#' 
#' This function creates a zip file of the entire class material directory, including all subdirectories and files.
#' 
#' @examples
#' #' \dontrun{save_zip_of_all_class_material()}
#' @export

save_zip_of_all_class_material <- function() {
  
  # Check if the backup directory exists, create it if not
  backup_folder_full_path <- file.path(get_class_material_root_path(), "backup")
  if (!dir.exists(backup_folder_full_path)) {
    dir.create(backup_folder_full_path, recursive = TRUE)
  }
  
  # Create a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create the backup folder name with timestamp
  backup_folder_name <- paste0("all_sds1000_class_material_", timestamp)
  
  backup_folder_path <- file.path(get_class_material_root_path(), "backup")
  
  # Zip the entire class material root path
  zip_file_name <- paste0(backup_folder_name, ".zip")
  zip_file_path <- file.path(backup_folder_path, zip_file_name)
  
  zip(zipfile = zip_file_path, files = get_class_material_root_path(), flags = "-r", extras = "-j")
  
}






