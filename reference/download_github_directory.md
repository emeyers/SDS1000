# Download files from a GitHub directory

This function downloads all files from a specified directory in the
class GitHub site

## Usage

``` r
download_github_directory(
  download_dir_name,
  show_existing_file_message = FALSE,
  show_nonexistent_file_message = FALSE
)
```

## Arguments

- github_directory_name:

  A string indicating the name of the directory in the class GitHub
  repository

## Examples

``` r
 # Download the first homework file
 if (FALSE) download_github_directory("homework/homework_01") # \dontrun{}
```
