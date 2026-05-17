# Restore a backup from a zip file

This function restores a backup from a specified zip file in the backup
directory. \#' @param backup_file_name The name of the backup zip file
to restore. \#' @return NULL

## Usage

``` r
restore_from_backup(backup_file_name, delete_backup_zip_file = TRUE)
```

## Examples

``` r
if (FALSE) restore_from_backup("homework/homework_-1_20250817_215216.zip") # \dontrun{}
```
