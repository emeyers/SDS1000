# Download any file related to the class

This function downloads any file related to the class from the class
GitHub repository.

## Usage

``` r
download_any_file(file_path_and_name, force_download = FALSE, mode = "wb")
```

## Arguments

- file_path_and_name:

  The name of the path and file to download.

- force_download:

  Will download and overwrite an existing file if the downloaded file
  has the same name.

- mode:

  A character indicating the mode with which to write the file. Useful
  values are "w", "wb" (binary), "a" (append) and "ab". Not used for
  methods "wget" and "curl". See also ‘Details’, notably about using
  "wb" for Windows. See download.file() mode argument.

## Examples

``` r
 # Download an image from the class GitHub repository
 if (FALSE) download_any_file("homework/homework_-1/homework_-1.Rmd") # \dontrun{}
```
