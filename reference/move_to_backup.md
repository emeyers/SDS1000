# Move a folder to the backup directory

This function moves a specified folder to a backup directory and zips it
with a timestamp. \#' @param folder_root_name The name of the folder to
be backed up. The folder must be one of the following: \#' "homework",
"class_code", "practice_session", "final_project", or "other". \#'
@param n An integer parameter that specifies the homework, class, or
practice session number. For final projects, this parameter is not used.
\#' @return NULL \#' @examples \#'

## Usage

``` r
move_to_backup(folder_root_name, n = NULL)
```
