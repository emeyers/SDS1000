

#' @import httr



# should set this to the name of the package
package_name <- "SDS1000"

# should set this to the github user name
github_user_name <- "emeyers"

# if the default branch is not "main", change this
branch_name <- "refs/heads/download_github_directory"    # "main"
#branch_name <- "main"

# should set this to the path where you want to save the files
class_material_root_path <- file.path(getwd(), "sds1000_material")




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


get_class_material_root_path <- function() {
  class_material_root_path
}


# could alternatively read these from ClassMaterials directory on GitHub
get_main_directory_names <- function() {
  c("homework", "class_code", "practice_session", "final_project", "other")
}


