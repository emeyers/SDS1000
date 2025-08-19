#' @import httr
#' @importFrom tools R_user_dir


# should set this to the name of the package
package_name <- "SDS1000"


# should set this to the github user name
github_user_name <- "emeyers"


# if the default branch is not "main", change this
#branch_name <- "main"
branch_name <- "download_github_directories"


#' Get the package name and version number
#'
#' Returns the name of the package and the version number. This is useful to
#' check that one is using the most recent version of the package in case the
#' package is updated in the middle of the semester.
#'
#' @examples
#'  # Download the first homework file
#'  get_version()
#'
#' @export
get_version <- function() {
  paste0(get_package_name(), ": version ", utils::packageVersion(get_package_name()))
}


#' Set the root path where class materials will be downloaded
#'
#' This function sets the root path where the sds1000_class_materials/ folder will be located.
#' By default, it sets the location to be in the Documents folder of a user on Windows and Mac,
#' and creates a Documents folder in the home directory of Unix users.
#' It also adds metadata listing where this directory is so that get_class_material_root_path()
#' works correctly after it is set across R sessions.
#'
#' @param dir_name A string indicating the directory where the class materials should be saved.
#'   If NULL, the default directory is used.
#' @export
set_class_material_root_path <- function(dir_name = NULL) {

  # Determine the default directory based on the OS
  if (is.null(dir_name)) {
    if (.Platform$OS.type == "windows") {
      dir_name <- normalizePath("~/")  #normalizePath("~/Documents")
    } else {
      dir_name <- normalizePath("~/Documents")
      if (!dir.exists(dir_name)) {
        dir.create(dir_name)
      }
    }
  }

  # Path to the configuration directory
  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }

  # Path to the configuration file
  config_file <- file.path(config_dir, "root_path.txt")


  # If the sds1000_material/ already exists and has content, move this content
  if (file.exists(config_file)) {
    
    # Read the existing path from the configuration file
    existing_path <- readLines(config_file, n = 1)
    
    # Check if the new path is the same as the existing path
    if (dir_name == existing_path) {
      message("Class material root path is already set to: ", dir_name)
      return(invisible(dir_name))
    } 
    
    
    # move existing files to the new directory
    message(paste("Changing class material root path from", existing_path, "to", dir_name))
    
    # Create the new directory if it doesn't exist
    if (!dir.exists(dir_name)) {
      dir.create(dir_name, recursive = TRUE)
    } 
    
    # Move existing sds1000_class_material files to the new root location
    old_materials_path <- file.path(existing_path, "sds1000_material")
    
    if (dir.exists(old_materials_path)) {
      
      if (!dir.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
      }
      
      dir_copy(old_materials_path, dir_name)
      
      # Remove the old directory after copying
      setwd(dir_name)  # ensure we are not in the directory to be deleted
      unlink(old_materials_path, recursive = TRUE)
      
      message(paste("Moved existing class materials from", old_materials_path, "to", file.path(dir_name, "sds1000_material")))
      
    }
    
  }
  
  
  # Write the selected directory to the configuration file
  writeLines(dir_name, config_file)

  message(paste("Class material root path set to:", dir_name))

  # Return the path invisibly
  invisible(dir_name)
}





#' Get the root path for class materials
#'
#' This function gets the root path where the class materials are stored.
#' If the path has not been set, it will be set to a default location.
#'
#' @return A string with the path to the root of the class materials directory.
#' @export
get_class_material_root_path <- function() {

  config_dir <- tools::R_user_dir("SDS1000", which = "config")
  config_file <- file.path(config_dir, "root_path.txt")

  if (file.exists(config_file)) {
    root_path <- readLines(config_file, n = 1)
  } else {
    message("Root path not set. Setting to default location.")
    root_path <- set_class_material_root_path()
  }

  file.path(root_path, "sds1000_material")
}






### Helper functions used throughout the package ---------------------------


get_package_name <- function() {
  package_name
}


get_github_user_name <- function() {
  github_user_name
}


get_base_url <- function() {
  base_path <- paste0("https://raw.githubusercontent.com/",
                      github_user_name, "/", package_name, "/", branch_name, "/ClassMaterial/")
  base_path
}



get_branch_name <- function() {
  branch_name
}



# could alternatively read these from ClassMaterials directory on GitHub
get_main_directory_names <- function() {
  c("homework", "class_code", "practice_session", "final_project", "other")
}
