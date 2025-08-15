

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




# Helper function that checks if a given file exists in the file_dir_name on the
# class github page. 

# Example: check_github_file_exists("homework/homework_-1", "test_data.rda")

check_github_file_exists <- function(file_dir_name, file_name) {

  file_names <- list_class_files(file_dir_name)

  if (!(file_name %in% file_names)) {
    stop(paste("The file",  file_name,
               "does not exit in the class GitHub repository."))
  }

}

