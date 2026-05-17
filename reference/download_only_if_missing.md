# A helper function that downloads a file only if it does not already exist. If the file already exists, a message is shown (if desired).

A helper function that downloads a file only if it does not already
exist. If the file already exists, a message is shown (if desired).

## Usage

``` r
download_only_if_missing(
  save_file_name,
  git_full_path,
  mode = "wb",
  show_existing_file_message = FALSE,
  show_nonexistent_file_message = FALSE
)
```

## Arguments

- save_file_name:

  The name of the file to save the downloaded file as.

- git_full_path:

  The full path to the file on GitHub.

- mode:

  A character indicating the mode with which to write the file. Useful
  values are "w", "wb" (binary), "a" (append) and "ab". Not used for
  methods "wget" and "curl". See also ‘Details’, notably about using
  "wb" for Windows. See download.file() mode argument.

- show_existing_file_message:

  A logical indicating whether to show a warning message if the file
  already exists.

- show_nonexistent_file_message:

  A logical indicating whether to show a message when downloading a file
  that does not already exist.

## Examples

``` r
 # Download a file if it doesn't already exist
 if (FALSE) download_only_if_missing("homework_-1.Rmd",
 "https://raw.githubusercontent.com/your_github_username/your_repo_name/main/ClassMaterial/homework/homework_-1/homework_-1.Rmd") # \dontrun{}
```
